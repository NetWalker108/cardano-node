usage_nomadpodman() {
  # Using a unique help message for all Nomad "sub-backends"
  usage_nomad
}

backend_nomadpodman() {

  op=${1:?$(usage_nomadpodman)}; shift

  case "${op}" in

    name )
      # Can be:
      # nomadpodman       (Using podman task driver in the cloud is not planned)
      # nomadexec    (Starts Nomad agents supporting the nix_installable stanza)
      # nomadcloud  (IOG Nomad Agents and Amazon S3 with credentials from Vault)
      echo 'nomadpodman'
    ;;

    # Sets jq envars "profile_container_specs_file" ,"nomad_environment",
    # "nomad_task_driver" and "one_tracer_per_node"
    setenv-defaults )
      local usage="USAGE: wb backend $op BACKEND-DIR"
      local backend_dir=${1:?$usage}; shift

      setenvjqstr 'nomad_task_driver'   "podman"
      setenvjqstr 'nomad_server_name'   "srv1"
      # As one task driver runs as a normal user and the other as a root, use
      # different names to allow restarting/reusing without cleaup, this way
      # data folders already there can be accessed without "permission denied"
      # errors.
      setenvjqstr 'nomad_client_name'   "cli1-pod"

      # Store the location of the Nix-built "container-specs" file.
      # TODO/FIXME: This is the only way to be able to later copy it to "$dir" ?
      local profile_container_specs_file
      profile_container_specs_file="${backend_dir}"/container-specs.json
      setenvjqstr 'profile_container_specs_file' "${profile_container_specs_file}"
      setenvjqstr 'nomad_environment'   "local"
      setenvjqstr 'one_tracer_per_node' "false"
    ;;

    allocate-run )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift

      # Copy the container specs file (container-specs.json)
      # This is the output file of the Nix derivation
      local profile_container_specs_file=$(envjqr 'profile_container_specs_file')
      # Create a nicely sorted and indented copy
      jq . "${profile_container_specs_file}" > "${dir}"/container-specs.json

      # Create nomad folder and copy the Nomad job spec file to run.
      mkdir -p "${dir}"/nomad
      # Create a nicely sorted and indented copy.
      jq -r ".nomadJob.podman.oneTracerPerCluster" \
        "${dir}"/container-specs.json              \
      > "${dir}"/nomad/nomad-job.json
      # The job file is "slightly" modified to suit the running environment.
      backend_nomad       allocate-run-nomad-job-patch-namespace "${dir}" "default"
      backend_nomadpodman allocate-run-nomad-job-patch-podman    "${dir}"
      # It needs to mount the tracer directory if "one_tracer_per_node" is
      # false, mount the genesis and CARDANO_MAINNET_MIRROR (if needed).
      nomad_job_file_create_mounts "${dir}"

      backend_nomad allocate-run "${dir}"
    ;;

    allocate-run-nomad-job-patch-podman )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      # Look up the OCI image's name and tag (Nix profile).
      local oci_image_name=$(jq -r .ociImage.imageName "${dir}"/container-specs.json)
      local oci_image_tag=$( jq -r .ociImage.imageTag  "${dir}"/container-specs.json)
      if podman image exists "${oci_image_name}:${oci_image_tag}"
      then
        setenvjqstr 'oci_image_was_already_available' "true"
        msg "OCI image ${oci_image_name}:${oci_image_tag} is already available"
      else
        setenvjqstr 'oci_image_was_already_available' "false"
        msg "Creating OCI image ..."
        # Script that creates the OCI image from nix2container layered output.
        local oci_image_skopeo_script=$(jq -r .ociImage.copyToPodman "${dir}"/container-specs.json)
        # TODO: for further research.
        # STORAGE_DRIVER=overlay "$oci_image_skopeo_script"
        # If podman 4.2.1 and nomad v1.3.5 this fix is not needed anymore
        # Forced the `overlay` storage driver or podman won't see the image.
        # https://docs.podman.io/en/latest/markdown/podman.1.html#note-unsupported-file-  systems-in-rootless-mode
        # Error was: workbench:  FATAL: OCI image registry.workbench.iog.io/  cluster:2l7wi7sh1zyp2mnl24m13ibnh2wsjvwg cannot be found by podman
        if ! "${oci_image_skopeo_script}"
        then
          fatal "Creation of OCI image ${oci_image_name}:${oci_image_tag} failed"
        else
          # Now check that `podman` can see the "cluster" OCI image.
          if ! podman image exists "${oci_image_name}:${oci_image_tag}"
          then
            fatal "OCI image ${oci_image_name}:${oci_image_tag} was created but cannot be found by podman"
          else
            msg "OCI image named \"${oci_image_name}:${oci_image_tag}\" created"
          fi
        fi
      fi
    ;;

    * )
      backend_nomad "${op}" "$@"
    ;;

  esac

}

nomad_job_file_create_mounts() {
    local dir=$1
    local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)
    local nomad_job_group_name=$(jq -r ". [\"job\"][\"${nomad_job_name}\"][\"group\"] | keys[0]" "${dir}"/nomad/nomad-job.json)
    local one_tracer_per_node=$(envjqr          'one_tracer_per_node')
    # If CARDANO_MAINNET_MIRROR is present generate a list of needed volumes.
    if test -n "${CARDANO_MAINNET_MIRROR}"
    then
      # The nix-store path contains 3 levels of symlinks. This is a hack to
      # avoid creating a container image with all these files.
      local immutable_store=$(readlink -f "${CARDANO_MAINNET_MIRROR}"/immutable)
      local mainnet_mirror_volumes="[
          \"${CARDANO_MAINNET_MIRROR}:${CARDANO_MAINNET_MIRROR}:ro\"
        , \"${immutable_store}:${immutable_store}:ro\"
        $(find -L "${immutable_store}" -type f -exec realpath {} \; | xargs dirname | sort | uniq | xargs -I "{}" echo ", \"{}:{}:ro\"")
      ]"
    else
      local mainnet_mirror_volumes="[]"
    fi
    # Hint:
    # - Working dir is: /tmp/cluster/
    # - Mount point is: /tmp/cluster/run/current
    ## The workbench is expecting an specific hierarchy of folders and files.
    local container_mountpoint=$(jq -r ". [\"job\"][\"${nomad_job_name}\"][\"meta\"][\"TASK_STATEDIR\"]" "${dir}"/nomad/nomad-job.json)
    # Nodes
    for node in $(jq_tolist 'keys' "${dir}"/node-specs.json)
    do
      local task_stanza_name="${node}"
      # Every node needs access to "./genesis/" and tracer when only 1 is used.
      local jq_filter="
        [
            \"${dir}/genesis:${container_mountpoint}/genesis:ro\"
          , \"${dir}/genesis/utxo-keys:${container_mountpoint}/genesis/utxo-keys:ro\"
        ]
        +
        (
          if \$one_tracer_per_node == true
          then
            [ ]
          else
            [ \"${dir}/tracer:${container_mountpoint}/tracer:rw\" ]
          end
        )
        +
        \$mainnet_mirror_volumes
      "
      local podman_volumes=$(jq "${jq_filter}" --argjson one_tracer_per_node "${one_tracer_per_node}" --argjson mainnet_mirror_volumes "${mainnet_mirror_volumes}" "${dir}"/profile/node-specs.json)
      jq ".job[\"${nomad_job_name}\"][\"group\"][\"${nomad_job_group_name}\"][\"task\"][\"${node}\"][\"config\"][\"volumes\"] = \$podman_volumes" --argjson podman_volumes "${podman_volumes}" "${dir}"/nomad/nomad-job.json | sponge "${dir}"/nomad/nomad-job.json
    done
    # Tracer
    if jqtest ".node.tracer" "${dir}"/profile.json && ! test "${one_tracer_per_node}" = "true"
    then
      local task_stanza_name_t="tracer"
      # Tracer only needs access to itself (its shared folder).
      local jq_filter_t="
        [
          \"${dir}/tracer:${container_mountpoint}/tracer:rw\"
        ]
      "
      local podman_volumes_t=$(jq "${jq_filter_t}" "${dir}"/profile/node-specs.json)
      jq ".job[\"${nomad_job_name}\"][\"group\"][\"${nomad_job_group_name}\"][\"task\"][\"tracer\"][\"config\"][\"volumes\"] = \$podman_volumes_t" --argjson podman_volumes_t "${podman_volumes_t}" "${dir}"/nomad/nomad-job.json | sponge "${dir}"/nomad/nomad-job.json
    fi
}
