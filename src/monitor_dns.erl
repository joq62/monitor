%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(monitor_dns).



%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("include/dns_data.hrl").
%% --------------------------------------------------------------------
%% External exports
-compile(export_all).
%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


init_dns_table()->
    Reply=case ets:info(?DNS_TABLE) of
	      undefined->
		  ets:new(?DNS_TABLE,[bag,named_table,public]);
	      _->
		  ets:delete(?DNS_TABLE),
		  ets:new(?DNS_TABLE,[bag,named_table,public])
	  end, 
    Reply.
     
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

update_dns_table(ServiceInfo)->
    {ZoneUpdate,ServiceIdUpdate,IpAddrUpdate,PortUpdate}=ServiceInfo,
    L=ets:match(?DNS_TABLE,'$1'),    
    L1=[{ZoneUpdate,ServiceIdUpdate,IpAddrUpdate,PortUpdate}||[{Zone,ServiceId,_,_}]<-L,
										       {ZoneUpdate,ServiceIdUpdate}=={Zone,ServiceId}],
    R=case L1 of
	  []->
	      ets:insert(?DNS_TABLE,ServiceInfo);
	  [Remove] ->
	      ets:delete_object(?DNS_TABLE,Remove),
	      ets:insert(?DNS_TABLE,ServiceInfo)
      end,
    R.


dns(ZoneWanted,ServiceIdWanted)->
    L=ets:lookup(?DNS_TABLE,ZoneWanted),
    R=[{IpAddr,Port}||{_,ServiceId,IpAddr,Port}<-L,
		      ServiceId=:=ServiceIdWanted],
    R.

dns(ServiceIdWanted)->
    L1=ets:match(?DNS_TABLE,{'_','$2','$3','$4'}),
    R=[{IpAddr,Port}||[ServiceId,IpAddr,Port]<-L1,
		      ServiceId=:=ServiceIdWanted],
    R.

delete(ServiceInfo)->
    ets:delete_object(?DNS_TABLE,ServiceInfo).


equal_dns(ServiceList_1,ServiceList_2)->
    [ServiceInfo_1||ServiceInfo_1<-ServiceList_1,ServiceInfo_2<-ServiceList_2,
		{?SERVICE_INFO_ZONE(ServiceInfo_1),?SERVICE_INFO_SERVICE(ServiceInfo_1)}=:={?SERVICE_INFO_ZONE(ServiceInfo_2),?SERVICE_INFO_SERVICE(ServiceInfo_2)}].

diff_dns(ServiceList_1,ServiceList_2)->
    Removed=diff_dns(ServiceList_1,ServiceList_2,[]),
    Added=diff_dns(ServiceList_2,ServiceList_1,[]),
    {Added,Removed}.

diff_dns([],_,Diff)->    
    Diff;
diff_dns([ServiceInfo|T],ServiceList_2,Acc) -> 
    R=[X||X<-ServiceList_2,
	  {?SERVICE_INFO_ZONE(ServiceInfo),?SERVICE_INFO_SERVICE(ServiceInfo)}=:={?SERVICE_INFO_ZONE(X),?SERVICE_INFO_SERVICE(X)}],
    case R of
	[]->
	    NewAcc=[ServiceInfo|Acc];
	_ ->
	    NewAcc=Acc
    end,
    diff_dns(T,ServiceList_2,NewAcc).
