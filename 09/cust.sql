delimiter //
drop table if exists assignment;
drop table if exists product;

create table product (id int primary key,
       current_owner_id int)//

create table assignment (id int primary key,
       product_id int,
       participant_id int,
       ts timestamp,
       action char(1),
       foreign key (product_id) references product(id) on delete cascade
       )//

drop procedure if exists get_chain_of_custody;
create procedure get_chain_of_custody(in product_id int)
begin
    select
    ass.id as n, pro.id as product, ass.participant_id as part, ass.ts, ass.action as act
    from
    assignment ass
    join product pro
    on (ass.product_id = pro.id)
    where pro.id = product_id
    order by ass.action, ass.ts;
end//

drop procedure if exists get_chains_of_custody;
create procedure get_chains_of_custody()
begin
    select
    ass.id as n, pro.id as product, ass.participant_id as part, ass.ts, ass.action as act
    from
    assignment ass
    join product pro
    on (ass.product_id = pro.id)
    order by pro.id, ass.action, ass.ts;
end//

delimiter ;

