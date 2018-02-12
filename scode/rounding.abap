
data: tp_deci3 type p decimals 3, 
tp_deci2 type p decimals 2. 
tp_deci3 = '123.456'. 


tp_deci2 = ( floor( tp_deci3 * 100 ) ) / 100. ""always rounded down 

tp_deci2 = ( ceil( tp_deci3 * 100 ) ) / 100. ""Always rounded up 

tp_deci2 = ( trunc( tp_deci3 * 100 ) ) / 100. 
