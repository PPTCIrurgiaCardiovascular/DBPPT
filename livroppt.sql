create database if not exists livroppt
default character set utf8mb4
default collate utf8mb4_general_ci;
use livroppt;
create table Proteses(
id varchar (10),
cod varchar (10),
posicao varchar (10),
fabricante varchar (15),
modelo varchar (30),
tamanho tinyint,
primary key (id)
);
insert into Proteses values
('emea19','3300TFX19','Aortica','Edwards','Magna Ease','19'),
('emea21','3300TFX21','Aortica','Edwards','Magna Ease','21'),
('emea23','3300TFX23','Aortica','Edwards','Magna Ease','23'),
('emea25','3300TFX25','Aortica','Edwards','Magna Ease','25'),
('emea27','3300TFX27','Aortica','Edwards','Magna Ease','27'),
('emem25','7300TFX25','Mitral','Edwards','Magna Ease','25'),
('emem27','7300TFX27','Mitral','Edwards','Magna Ease','27'),
('emem29','7300TFX29','Mitral','Edwards','Magna Ease','29'),
('emem31','7300TFX31','Mitral','Edwards','Magna Ease','31'),
('emem33','7300TFX33','Mitral','Edwards','Magna Ease','33'),
('atgt19','TFGT-19A','Aortica','Abbott','Trifecta GT','19'),
('atgt21','TFGT-21A','Aortica','Abbott','Trifecta GT','21'),
('atgt23','TFGT-23A','Aortica','Abbott','Trifecta GT','23'),
('atgt25','TFGT-25A','Aortica','Abbott','Trifecta GT','25'),
('atgt27','TFGT-27A','Aortica','Abbott','Trifecta GT','27'),
('atgt29','TFGT-29A','Aortica','Abbott','Trifecta GT','29'),
('aepm25','E100-25M-00','Mitral','Abbott','Epic','25'),
('aepm27','E100-27M-00','Mitral','Abbott','Epic','27'),
('aepm29','E100-29M-00','Mitral','Abbott','Epic','29'),
('aepm31','E100-31M-00','Mitral','Abbott','Epic','31'),
('aepm33','E100-33M-00','Mitral','Abbott','Epic','33');

alter table proteses
modify column cod varchar (20);

create table enxertos(
cod varchar (10),
descricao text,
primary key (cod)
);
describe enxertos;

select * from Proteses;

create table oxigenador(
cod varchar (10),
fabricante varchar (10),
modelo varchar (10),
primary key (cod)
);
insert into oxigenador values
('bra','Braile','Adulto'),
('med','Medtronic','Affinity'),
('liv','LivaNova','Inspire'),
('maq','Maquet','Quadrox i'),
('nib','Nipro','BioCube'),
('nbr','Nipro','Brizio'),
('niv','Nipro','Vital');

select * from oxigenador;
create table hospitais(
cod varchar (5),
nome varchar (50),
primary key (cod)
);
describe hospitais;
insert into hospitais values
('CHU','Centro Hospitalar Unimed'),
('HDH','Hospital e Maternidade Dona Helena'),
('HUL','Hospital Unimed Litoral - Balneario Camboriu'),
('HRHDS','Hospital Regional Hans Dieter Schmidt'),
('HIAE','Hospital Israelita Albert Einstein');
select * from hospitais;
alter table cirurgias
modify column Cirurgiao varchar(5);
alter table cirurgias
add foreign key (Cirurgiao)
references cirurgioes(cod);
alter table cirurgias
add foreign key (hospital)
references hospitais(cod);
describe cirurgias;
select * from cirurgias;

INSERT INTO cirurgias VALUES (DEFAULT, 'Fábio Bastos Pope', 'M', '1978-02-10', '2020-05-20', '1', 'Unimed', 'CHU', 'med', 'Custodiol', '0.96', 'El', 'I35.0', 'TVAo', 'MICS', 'atgt25', NULL, 'APT', 'Vivo');
INSERT INTO cirurgias VALUES (DEFAULT, 'Renato Bastos Pope', 'M', '1975-12-06', '2020-06-15', '1','Unimed', 'CHU', 'med', 'Custodiol', '0.96', 'El', 'I35.0', 'TVAo', 'MICS', 'emea27', NULL, 'RP', 'Vivo'),
(Default,'Leonardo Zozula Blind','M','1978-01-24','2020-07-18','0','SUS','HRHDS',NULL,NULL,'1.2','El','Q21.1','Fechamento de CIA','Endovasc',NULL,NULL,'RBP','Vivo');
alter table cirurgias

create table cirurgioes(
cod varchar (5),
nome varchar (50),
primary key (cod)
);
insert into cirurgioes values('RBP','Dr Renato Bastos Pope'),
('APT','Dr Alisson Parrilha Toschi'),
('RP','Dr Robinson Poffo'),
('ARF','Dr Ademar Regueira Filho'),
('MB','Dr Marcos Bonin'),
('MBB','Dr Mateus Bueno Bueno'),('CAS','Dr Cezar Suchard'),
('VC','Dr Victor Clementoni');
select * from cirurgioes;
select * from cirurgias;

create table cirurgias(
id int not null auto_increment,
nome varchar(50),
gen enum('M','F'),
DN date,
DC date,
CEC boolean,
Convenio varchar(15),
Hospital varchar (5),
Oxigenador varchar (10),
Cardioplegia enum ('Braile','Custodiol','Del Nido','St Thomas'),
Euroscore decimal (4,2),
Carater enum ('El','Ur','Em'),
CID_10 VARCHAR (5),
Cirurgia text,
Tipo enum ('MICS','Endovasc','Conv'),
Proteses varchar (10),
Enxertos varchar (30),
Cirurgiao varchar (5),
Desfecho enum ('Vivo','Obito'),
primary key (id),
foreign key (Proteses) references proteses(id),
foreign key (Enxertos) references enxertos(cod),
foreign key (Hospital) references hospitais(cod),
foreign key (Cirurgiao) references cirurgioes(cod),
foreign key (Oxigenador) references oxigenador(cod)
);

select * from proteses;
select * from cirurgias;