usage_nomadexec() {
  # Using a unique help message for all Nomad "sub-backends"
  usage_nomad
}

backend_nomadexec() {

  op=${1:?$(usage_nomadexec)}; shift

  case "${op}" in

    name )
      # Can be:
      # nomadpodman       (Using podman task driver in the cloud is not planned)
      # nomadexec    (Starts Nomad agents supporting the nix_installable stanza)
      # nomadcloud  (IOG Nomad Agents and Amazon S3 with credentials from Vault)
      echo 'nomadexec'
    ;;

    # Sets jq envars "profile_container_specs_file" ,"nomad_environment",
    # "nomad_task_driver" and "one_tracer_per_node"
    setenv-defaults )
      local usage="USAGE: wb backend $op BACKEND-DIR"
      local backend_dir=${1:?$usage}; shift

      setenvjqstr 'nomad_task_driver'   "exec"
      setenvjqstr 'nomad_server_name'   "srv1"
      # As one task driver runs as a normal user and the other as a root, use
      # different names to allow restarting/reusing without cleaup, this way
      # data folders already there can be accessed without "permission denied"
      # errors.
      setenvjqstr 'nomad_client_name'   "cli1-exe"

      # Store the location of the Nix-built "container-specs" file.
      # TODO/FIXME: This is the only way to be able to later copy it to "$dir" ?
      local profile_container_specs_file
      profile_container_specs_file="${backend_dir}"/container-specs.json
      setenvjqstr 'profile_container_specs_file' "${profile_container_specs_file}"
      setenvjqstr 'nomad_environment'   "local"
      setenvjqstr 'one_tracer_per_node' "true"
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
      jq -r ".nomadJob.exec.oneTracerPerNode"      \
        "${dir}"/container-specs.json              \
      > "${dir}"/nomad/nomad-job.json
      # The job file is "slightly" modified to suit the running environment.
      backend_nomad allocate-run-nomad-job-patch-namespace "${dir}" "default"
      backend_nomad allocate-run-nomad-job-patch-nix       "${dir}"

      backend_nomad allocate-run "${dir}"
    ;;

    * )
      backend_nomad "${op}" "$@"
    ;;

  esac

}
