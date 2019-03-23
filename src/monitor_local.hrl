%% service_info
%% Information of the servie
-record(state, {service_list,git_url,kubelet_info,lSock,max_workers,active_workers,workers,dns_list,dns_addr}).
