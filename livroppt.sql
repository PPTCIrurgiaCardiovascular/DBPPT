create database if not exists livroppt;
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
('emem33','7300TFX33','Mitral','Edwards','Magna Ease','33');

select * from Proteses;

create table cirurgias(
id int not null auto_increment,
nome varchar(50),
gen enum('M','F'),
DN date,
DC date,
CEC boolean,
Convenio varchar(15),
Hospital enum ('CHU','HDH','HIAE','HUL','HRHDS'),
Oxigenador enum ('Braile','LivaNova','Maquet','Medtronic','Nipro'),
Cardioplegia enum ('Braile','Custodiol','Del Nido','St Thomas'),
Euroscore decimal (4,2),
Carater enum ('El','Ur','Em'),
CID_10 VARCHAR (5),
Cirurgia text,
Tipo enum ('MICS','Endovasc','Conv'),
Proteses varchar (10),
Enxertos varchar (30),
Cirurgiao enum ('Dr Renato Bastos Pope','Dr Alisson Parrilha Toschi','Dr Robinson Poffo','Dr Ademar Regueira Filho','Dr Marcos Bonin','Dr Mateus Bueno Bueno','Dr Cezar Suchard','Dr Victor Clementoni'),
Desfecho enum ('Vivo','Obito'),
primary key (id),
foreign key (Proteses) references Proteses(id)
);
describe cirurgias;