side:`bid`ask

quotes:([]
 time:`timestamp$();
 sym:`$();
 id:`u#`long$();
 side:`side$();
 size:`long$();
 price:`float$())

upd:{-3!x;`quotes upsert .z.p,'x}
\p 5042

count select from quotes where side=`bid

upd[enlist (`XBTUSD;1;`buy;12;1.2)]
