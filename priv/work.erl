todo : ne pas avoir plusieurs clauses pour une même technic


crush p:Paysan(y) Ble(y*2) (delay: x/1000) -> Void(0) end

mod:crush({y,Var_y},{x,Var_x},{'Paysan',Var_p},{'Ble',_},State) ->
    case kraft:consumeComposants([{'Paysan',Var_y},{'Ble',Var_y*2}],State)
        of false -> ok
         ; true ->
            schedule( draw( Var_y,
                            [ {getProp(Var_p,skillAgricole)*2,{return,'Farine',Var_x/9}}
                            , {force,'Void',0}
                            ]
                          ),
                      Var_x/1000)
    end.

mod:signature(crush) -> {[y,x,{'Paysan',p}], ['Paysan','Ble'], ['Farine','Void']}


:callTechnic(boulangerie,crush,{'Paysan','Ble'},[{x,100}],[{'Paysan',P}],Conn)
