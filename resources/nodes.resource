
{sasl_node, [
    {startup, [{handler, systest_slave},
               {link_to_parent, true},
               {cover, false}]},
    {flags, [{start,
              "-boot start_sasl +W w +K true +A30 +P 1048576"
              " -pa ${settings.base_dir}"
              " -pa ${settings.base_dir}/ebin"
              " -mnesia dir '\"${ct.priv_dir}/${proc.id}-mnesia\"'"
              " -mnesia dump_log_write_threshold 50000"
              " -mnesia dc_dump_limit 40"}]},
    {apps, [{sasl, [{errlog_type, info}]}]},
    {on_join, [{local, mnesia_tx_check, start, []}]}
]}.

{cluster, [
    {sut, [{localhost, [node1, node2, node3, node4, node5, node6]}]},
    {processes, [{node1, [sasl_node]},
                 {node2, [sasl_node]},
                 {node3, [sasl_node]},
                 {node4, [sasl_node]},
                 {node5, [sasl_node]},
                 {node6, [sasl_node]}]},
    {on_start, [{local, mnesia_tx_check, on_start, []}]}
]}.

{sync_tx_races_SUITE, [{all, cluster}]}.

