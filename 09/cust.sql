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

drop procedure if exists find_wrong_owner_id;
create procedure find_wrong_owner_id()
begin
    select p.id, p.current_owner_id, a.oid as should_be_owner_id
    from product as p
    join
        (select product_id as pid, participant_id as oid
        from assignment
        where (product_id, ts, action) in
            (select product_id, max(ts), max(action)
            from assignment
            group by product_id)
        ) as a
    on (p.id = a.pid and p.current_owner_id != a.oid);
end//

drop procedure if exists correct_owner_id;
create procedure correct_owner_id()
begin
    DECLARE done INT DEFAULT FALSE;
    DECLARE pid, cid, oid INT;
    DECLARE cur1 CURSOR FOR
        select p.id, p.current_owner_id, a.oid as should_be_owner_id
        from product as p
        join
            (select product_id as pid, participant_id as oid
            from assignment
            where (product_id, ts, action) in
                (select product_id, max(ts), max(action)
                from assignment
                group by product_id)
            ) as a
        on (p.id = a.pid and p.current_owner_id != a.oid);
    DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;
    OPEN cur1;
    read_loop: LOOP
        FETCH cur1 INTO pid, cid, oid;
        IF done THEN
            LEAVE read_loop;
        END IF;
        UPDATE product SET current_owner_id = oid WHERE id = pid;
    END LOOP;
    CLOSE cur1;
end//

delimiter ;

