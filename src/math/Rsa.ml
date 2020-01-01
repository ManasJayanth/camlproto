open! Base

module Make (Platform: PlatformTypes.S) = struct
  module Bigint = Bigint.Make(Platform)

  type pub = {
    n: Cstruct.t;
    e: Cstruct.t; (* 65537 *)
  }

  module RsaKey = struct
    type t = { n: Bigint.t; e: Bigint.t }
    let create (key: pub) =
      (* Caml.print_endline "rsa create key"; *)
      { n = Bigint.of_cstruct_be key.n; e = Bigint.of_cstruct_be key.e }
    let encrypt ~key data =
      Bigint.powm (Bigint.of_cstruct_be data) key.e key.n
        |> Bigint.to_cstruct_be
  end

  let default_keys: pub list = [
    (* Taken from JuanPotato's vail *)
    { e = Cstruct.of_hex "010001"; n = Cstruct.of_hex "C150023E2F70DB7985DED064759CFECF0AF328E69A41DAF4D6F01B538135A6F91F8F8B2A0EC9BA9720CE352EFCF6C5680FFC424BD634864902DE0B4BD6D49F4E580230E3AE97D95C8B19442B3C0A10D8F5633FECEDD6926A7F6DAB0DDB7D457F9EA81B8465FCD6FFFEED114011DF91C059CAEDAF97625F6C96ECC74725556934EF781D866B34F011FCE4D835A090196E9A5F0E4449AF7EB697DDB9076494CA5F81104A305B6DD27665722C46B60E5DF680FB16B210607EF217652E60236C255F6A28315F4083A96791D7214BF64C1DF4FD0DB1944FB26A2A57031B32EEE64AD15A8BA68885CDE74A5BFC920F6ABF59BA5C75506373E7130F9042DA922179251F" };
    { e = Cstruct.of_hex "010001"; n = Cstruct.of_hex "AEEC36C8FFC109CB099624685B97815415657BD76D8C9C3E398103D7AD16C9BBA6F525ED0412D7AE2C2DE2B44E77D72CBF4B7438709A4E646A05C43427C7F184DEBF72947519680E651500890C6832796DD11F772C25FF8F576755AFE055B0A3752C696EB7D8DA0D8BE1FAF38C9BDD97CE0A77D3916230C4032167100EDD0F9E7A3A9B602D04367B689536AF0D64B613CCBA7962939D3B57682BEB6DAE5B608130B2E52ACA78BA023CF6CE806B1DC49C72CF928A7199D22E3D7AC84E47BC9427D0236945D10DBD15177BAB413FBF0EDFDA09F014C7A7DA088DDE9759702CA760AF2B8E4E97CC055C617BD74C3D97008635B98DC4D621B4891DA9FB0473047927" };
    { e = Cstruct.of_hex "010001"; n = Cstruct.of_hex "BDF2C77D81F6AFD47BD30F29AC76E55ADFE70E487E5E48297E5A9055C9C07D2B93B4ED3994D3ECA5098BF18D978D54F8B7C713EB10247607E69AF9EF44F38E28F8B439F257A11572945CC0406FE3F37BB92B79112DB69EEDF2DC71584A661638EA5BECB9E23585074B80D57D9F5710DD30D2DA940E0ADA2F1B878397DC1A72B5CE2531B6F7DD158E09C828D03450CA0FF8A174DEACEBCAA22DDE84EF66AD370F259D18AF806638012DA0CA4A70BAA83D9C158F3552BC9158E69BF332A45809E1C36905A5CAA12348DD57941A482131BE7B2355A5F4635374F3BD3DDF5FF925BF4809EE27C1E67D9120C5FE08A9DE458B1B4A3C5D0A428437F2BECA81F4E2D5FF" };
    { e = Cstruct.of_hex "010001"; n = Cstruct.of_hex "B3F762B739BE98F343EB1921CF0148CFA27FF7AF02B6471213FED9DAA0098976E667750324F1ABCEA4C31E43B7D11F1579133F2B3D9FE27474E462058884E5E1B123BE9CBBC6A443B2925C08520E7325E6F1A6D50E117EB61EA49D2534C8BB4D2AE4153FABE832B9EDF4C5755FDD8B19940B81D1D96CF433D19E6A22968A85DC80F0312F596BD2530C1CFB28B5FE019AC9BC25CD9C2A5D8A0F3A1C0C79BCCA524D315B5E21B5C26B46BABE3D75D06D1CD33329EC782A0F22891ED1DB42A1D6C0DEA431428BC4D7AABDCF3E0EB6FDA4E23EB7733E7727E9A1915580796C55188D2596D2665AD1182BA7ABF15AAA5A8B779EA996317A20AE044B820BFF35B6E8A1" };
    { e = Cstruct.of_hex "010001"; n = Cstruct.of_hex "BE6A71558EE577FF03023CFA17AAB4E6C86383CFF8A7AD38EDB9FAFE6F323F2D5106CBC8CAFB83B869CFFD1CCF121CD743D509E589E68765C96601E813DC5B9DFC4BE415C7A6526132D0035CA33D6D6075D4F535122A1CDFE017041F1088D1419F65C8E5490EE613E16DBF662698C0F54870F0475FA893FC41EB55B08FF1AC211BC045DED31BE27D12C96D8D3CFC6A7AE8AA50BF2EE0F30ED507CC2581E3DEC56DE94F5DC0A7ABEE0BE990B893F2887BD2C6310A1E0A9E3E38BD34FDED2541508DC102A9C9B4C95EFFD9DD2DFE96C29BE647D6C69D66CA500843CFAED6E440196F1DBE0E2E22163C61CA48C79116FA77216726749A976A1C4B0944B5121E8C01" };

    { e = Cstruct.of_hex "010001"; n = Cstruct.of_hex "C6AEDA78B02A251DB4B6441031F467FA871FAED32526C436524B1FB3B5DCA28EFB8C089DD1B46D92C895993D87108254951C5F001A0F055F3063DCD14D431A300EB9E29517E359A1C9537E5E87AB1B116FAECF5D17546EBC21DB234D9D336A693EFCB2B6FBCCA1E7D1A0BE414DCA408A11609B9C4269A920B09FED1F9A1597BE02761430F09E4BC48FCAFBE289054C99DBA51B6B5EB7D9C3A2AB4E490545B4676BD620E93804BCAC93BF94F73F92C729CA899477FF17625EF14A934D51DC11D5F8650A3364586B3A52FCFF2FEDEC8A8406CAC4E751705A472E55707E3C8CD5594342B119C6C3293532D85DBE9271ED54A2FD18B4DC79C04A30951107D5639397" };
    { e = Cstruct.of_hex "010001"; n = Cstruct.of_hex "B1066749655935F0A5936F517034C943BEA7F3365A8931AE52C8BCB14856F004B83D26CF2839BE0F22607470D67481771C1CE5EC31DE16B20BBAA4ECD2F7D2ECF6B6356F27501C226984263EDC046B89FB6D3981546B01D7BD34FEDCFCC1058E2D494BDA732FF813E50E1C6AE249890B225F82B22B1E55FCB063DC3C0E18E91C28D0C4AA627DEC8353EEE6038A95A4FD1CA984EB09F94AEB7A2220635A8CEB450EA7E61D915CDB4EECEDAA083AA3801DAF071855EC1FB38516CB6C2996D2D60C0ECBCFA57E4CF1FB0ED39B2F37E94AB4202ECF595E167B3CA62669A6DA520859FB6D6C6203DFDFC79C75EC3EE97DA8774B2DA903E3435F2CD294670A75A526C1" };
    { e = Cstruct.of_hex "010001"; n = Cstruct.of_hex "C2A8C55B4A62E2B78A19B91CF692BCDC4BA7C23FE4D06F194E2A0C30F6D9996F7D1A2BCC89BC1AC4333D44359A6C433252D1A8402D9970378B5912B75BC8CC3FA76710A025BCB9032DF0B87D7607CC53B928712A174EA2A80A8176623588119D42FFCE40205C6D72160860D8D80B22A8B8651907CF388EFFBEF29CD7CF2B4EB8A872052DA1351CFE7FEC214CE48304EA472BD66329D60115B3420D08F6894B0410B6AB9450249967617670C932F7CBDB5D6FBCCE1E492C595F483109999B2661FCDEEC31B196429B7834C7211A93C6789D9EE601C18C39E521FDA9D7264E61E518ADD6F0712D2D5228204B851E13C4F322E5C5431C3B7F31089668486AADC59F" };
  ]

  module RsaManager = struct
    type fingerprint = int64
    type key = { key: RsaKey.t; fingerprint: fingerprint }
    type t = key list

    exception FingerprintsNotFound of fingerprint list

    module TLR = TLRuntime
    module M = TLGen.MTProto

    let calc_fingerprint { n; e } =
      let cs = TLR.Encoder.encode M.TL_rsa_public_key.encode { n; e }
        |> TLR.Encoder.to_cstruct
        |> Platform.Crypto.SHA1.digest
      in
      (* let finger_cs = Cstruct.sub cs 12 8 in
      Caml.print_endline "New fingerprint";
      Cstruct.hexdump finger_cs; *)
      Cstruct.LE.get_uint64 cs 12

    let create_with_pub_keys (l: pub list) = List.map l ~f:(fun k -> {
      key = RsaKey.create k;
      fingerprint = calc_fingerprint k;
    })

    let default = create_with_pub_keys default_keys

    let find_by_fingerprint t finger =
      match List.find t ~f:(fun k -> Int64.(k.fingerprint = finger)) with
      | Some x -> x
      | None -> raise @@ FingerprintsNotFound [finger]

    let find_by_fingerprints t fingers =
      let open Option.Monad_infix in
      match List.find_map t ~f:(fun key ->
        List.find fingers ~f:(fun finger -> Int64.(key.fingerprint = finger))
          >>| fun found_finger -> key, found_finger)
      with
      | Some x -> x
      | None -> raise @@ FingerprintsNotFound fingers

    let encrypt ~key:{ key; _ } data = RsaKey.encrypt ~key data
  end
end
