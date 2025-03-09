{
  boot.kernel.sysctl = {
    # Sysctl customizations from steamos
    "kernel.sched_cfs_bandwidth_slice_us" = 3000;
    # Lower the fin timeout to let games reuse their ports
    # if they're killed and restarted too quickly
    "net.ivp4.tcp_fin_timeout" = 5;
    # Prevents slowdowns in case games experience split locks
    "kernel.split_lock_mitigate" = 0;
    
    "vm.max_map_count" = 2147483642;
  };
}
