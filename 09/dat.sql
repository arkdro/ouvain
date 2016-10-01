-- products
insert into product (id, current_owner_id) values (1, 10);
insert into product (id, current_owner_id) values (2, 10);
-- conflicts with assignment participant id
insert into product (id, current_owner_id) values (3, 20);

insert into product (id, current_owner_id) values (4, 5);

-- assignments
insert into assignment (id, product_id, participant_id, ts, action) values (1, 1, 5, '2016-09-25 10:00:01', 'c');
insert into assignment (id, product_id, participant_id, ts, action) values (2, 1, 10, '2016-09-25 10:00:01', 'm');
insert into assignment (id, product_id, participant_id, ts, action) values (3, 2, 5, '2016-09-25 15:00:01', 'c');
insert into assignment (id, product_id, participant_id, ts, action) values (4, 2, 20, '2016-09-25 15:00:01', 'm');
insert into assignment (id, product_id, participant_id, ts, action) values (5, 2, 10, '2016-09-25 15:30:00', 'm');

insert into assignment (id, product_id, participant_id, ts, action) values (6, 3, 5, '2016-09-26 10:00:00', 'c');
-- conflicts with product current owner id
insert into assignment (id, product_id, participant_id, ts, action) values (7, 3, 5, '2016-09-26 10:00:00', 'm');

insert into assignment (id, product_id, participant_id, ts, action) values (8, 4, 5, '2016-09-26 16:00:00', 'c');
