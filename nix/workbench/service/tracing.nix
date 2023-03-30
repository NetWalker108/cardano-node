{ nodeSpec
, tracer
}:
{
  UseTraceDispatcher   = true;

  ## Please see the generated tracing configuration reference at:
  ##
  ## https://github.com/input-output-hk/cardano-node/blob/master/doc/new-tracing/tracers_doc_generated.md#trace-messages
  ##
  TraceOptions  = {
    "" =
      { severity = "Info";
        backends = [
          "Stdout MachineFormat"
          "EKGBackend"
        ] ++ (if !tracer then [] else
          [
            "Forwarder"
          ]);
      };

    ## These are comparision-specific config deviations from the default.
    ##
    "Resources".backends = ["EKGBackend"];
    "Net.Net.AcceptPolicy".severity = "Info";
    "BlockFetch.Client".severity = "Silence";
    "BlockFetch.Decision".severity = "Silence";
    "BlockFetch.Remote".severity = "Silence";
    "BlockFetch.Remote.Serialised".severity = "Silence";
    "BlockFetch.Server".severity = "Silence";
    "ChainDB".severity = "Info";
    "ChainSync.Client".severity = "Silence";
    "ChainSync.Remote".severity = "Silence";
    "ChainSync.Remote.Serialised".severity = "Silence";
    "ChainSync.Remote.ServerBlock".severity = "Silence";
    "ChainSync.Remote.ServerHeader".severity = "Silence";
    "Net.ConnectionManager.Local".severity = "Info";
    "Net.ConnectionManager.Remote".severity = "Info";
    "Net.DNSResolver".severity = "Info";
    "Net.Subscription.DNS".severity = "Info";
    "Net.Startup.DiffusionInit".severity = "Info";
    "Net.ErrorPolicy.Remote".severity = "Info";
    "Forge.Loop".severity = "Info";
    "Net.Handshake.Remote".severity = "Silence";
    "Net.Subscription.IP".severity = "Info";
    "Net.InboundGovernor".severity = "Info";
    "ChainSync.Local".severity = "Silence";
    "Net.ErrorPolicy.Local".severity = "Info";
    "Net.Handshake.Local".severity = "Silence";
    "Net.Peers".severity = "Info";
    "TxSubmission".severity = "Silence";
    "Mempool".severity = "Info";
    "Net.Mux".severity = "Silence";
    "Net.Mux.Remote".severity = "Silence";
    "Net.PeerSelection".severity = "Info";
    "Net.Peers.PublicRoot".severity = "Info";
    "Net.Server".severity = "Info";
    "TxSubmission.TxInbound".severity = "Silence";
    "TxSubmission.TxOutbound".severity = "Silence";
  };
}
