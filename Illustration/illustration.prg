wfclose(noerr) .\data_for_illustration.wf1
wfopen .\data_for_illustration.wf1

smpl @first 2019q4
equation eq.ls dlog(ip) c dlog(ip(-1)) dlog(gdp)  libor*dum_fincrisis

eq.fcastdecomp(noprompt)

eq.fcastdecomp(include_sum="t")

eq.fcastdecomp(scenarios="_sd",sample="2020q1 2025q4")

eq.fcastdecomp(scenarios="_sd _bl",sample="2020q1 2025q4")

