
CREATE SEQUENCE users_id_sequence;
/* Create test tables */
CREATE TABLE users (
       id int DEFAULT nextval('users_id_sequence') PRIMARY KEY,
       first_name varchar(80),
       last_name varchar(80),
       high_score int,
       active boolean,
       datablob bytea,
       created timestamp
);

--GRANT ALL PRIVILEGES ON TABLE users TO itest;
-- GRANT ALL PRIVILEGES ON SEQUENCE users_id_sequence TO itest;

CREATE TABLE nodes (
       id char(32) PRIMARY KEY,
       authz_id char(32) UNIQUE NOT NULL,
       org_id char(32) NOT NULL,
       name varchar(255) NOT NULL,
       environment varchar(255) NOT NULL,
       serialized_object bytea,
       last_updated_by char(32) NOT NULL,
       created_at timestamp NOT NULL,
       updated_at timestamp NOT NULL
);
