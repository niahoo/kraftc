
"crush p:Paysan() Ble(1) (delay: 123) -> Farine(1) {quality = p.skill} end"



callTechnic(boulangerie,crush,{'Paysan','Ble'},Conn)

callTechnic(M,F,Signature,Conn)
  MM = atomCat('km$', M) %% 'km$boulangerie'
  Quantities = MM:technic_qtts(F,Signature),
  case kraft:get_composants(Quantities, Conn#conn.client)
    of false -> abandon %% On s'arrête là
     ; {ok, Composants} ->  % Composants = [{'Paysan',P},{'Ble',B}]
        case apply(M,F,Composants)
          of abandon ->
              abandon
           ; {TypeResults,Delay} ->
              kraft:shedule_release(TypeResults,Delay,Conn)
        end
  end.



-module('km$boulangerie').
-export([crush/2]).

technic_info(crush) -> [
      {{'Paysan','Ble'}, ['Farine','Void']}
    ].

technic_qtts(crush,{'Paysan','Ble'}) -> [{'Paysan',service},{'Ble',1}].

crush({'Paysan',_p},{'Ble',_}) ->
 -> %% Le Blé n'a pas de variable défine
            _p_skill = getprop(_p,skill),
            _Farine_quality = {quality,_p_skill},
            { [ {'Farine', 1, [_Farine_quality]}]
            , _Delay = 123
            }
    end.

