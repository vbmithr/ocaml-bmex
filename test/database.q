side:`bid`ask

quotes:([]
 time:`timestamp$();
 sym:`$();
 id:`long$();
 side:`side$();
 size:`long$();
 price:`float$())

upd:{-3!x;`quotes upsert .z.p,'x}
\p 5042

upd[enlist (`XBTUSD;1;`buy;12;1.2)]
