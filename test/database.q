side:`bid`ask

quotes:([]
 time:`timestamp$();
 sym:`$();
 id:`long$();
 side:`side$();
 size:`long$();
 price:`float$())

upd:{`quote upsert x}
