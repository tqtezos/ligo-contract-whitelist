

(* ------ *)
(* Types: *)
(* ------ *)

(* type WhitelistId = Natural *)
type whitelist_id_t = nat

(* data OutboundWhitelists = OutboundWhitelists *)
(*   { restricted        :: !Bool *)
(*   , allowedWhitelists :: !(Set WhitelistId) *)
(*   } *)
type outbound_whitelists_t = {
  unrestricted : bool;
  allowed_whitelists : whitelist_id_t set;
}

(* data WhitelistOutboundParams = WhitelistOutboundParams *)
(*   { whitelist             :: !WhitelistId *)
(*   , newOutboundWhitelists :: !(Maybe OutboundWhitelists) *)
(*   } *)
type whitelist_outbound_params_t = {
  whitelist_id : whitelist_id_t;
  new_outbound_whitelists : outbound_whitelists_t option;
}

(* data NewUserParams a = NewUserParams *)
(*   { newUser          :: !a *)
(*   , newUserWhitelist :: !(Maybe WhitelistId) *)
(*   } *)
type new_user_params_t = {
  new_user : address;
  new_user_whitelist : whitelist_id_t option;
}

(* data TransferParams a = TransferParams { from :: !a, to :: !a } *)
type transfer_params_t = {
  from: address ;
  to_: address ;
}

(* data Parameter' a *)
(*   = SetIssuer { newIssuer :: !a } *)
(*   | AddUser { newUserParams :: !(NewUserParams a) } *)
(*   | SetWhitelistOutbound { whitelistOutboundParams :: !WhitelistOutboundParams } *)
(*   | SetAdmin { admin :: !Address } *)
(*   | GetIssuer { viewIssuer :: !(View_ a) } *)
(*   | GetUser { viewUser :: !(View a (Maybe WhitelistId)) } *)
(*   | GetWhitelist { viewWhitelist :: !(View WhitelistId (Maybe OutboundWhitelists)) } *)
(*   | GetAdmin { viewAdmin :: !(View_ Address) } *)
type other_params_t =
  SetIssuer of address
| AddUser of new_user_params_t
| SetWhitelistOutbound of whitelist_outbound_params_t
| SetAdmin of address
| GetIssuer of (unit * address contract)
| GetUser of (address * whitelist_id_t option contract)
| GetWhitelist of (whitelist_id_t * outbound_whitelists_t option contract)
| GetAdmin of (unit * address contract)

(* data Parameter a *)
(*   = AssertTransfer { transferParams :: !(TransferParams a) } *)
(*   | OtherParameter { otherParams :: !(Parameter' a) } *)
type parameter_t =
  AssertTransfer of transfer_params_t
| OtherParameter of other_params_t

(* type Users a = BigMap a WhitelistId *)
type users_t = (address, whitelist_id_t) big_map

(* type Whitelists = BigMap WhitelistId OutboundWhitelists *)
type whitelists_t = (whitelist_id_t, outbound_whitelists_t) big_map

(* data Storage a = *)
(*   Storage *)
(*     { issuer :: !a *)
(*     , users :: !(Users a) *)
(*     , whitelists :: !Whitelists *)
(*     , admin :: !Address *)
(*     } *)
type storage_t = {
  issuer : address;
  users : users_t;
  whitelists : whitelists_t;
  admin : address;
}

type return_t = operation list * storage_t


(* ----- *)
(* Utils *)
(* ----- *)

let no_op: operation list = []

(* -- | Assert sender is the given address or fail with an error *)
let assert_admin (storage : storage_t) : storage_t =
  if (Tezos.sender = storage.admin) then
    storage
  else
    (failwith "only admin may update" : storage_t)

(* -- | Specialized `update` *)
(* addUserWhitelist :: forall a s. IsComparable a => a & Maybe WhitelistId & Users a & s :-> Users a & s *)
let add_user_whitelist (user, whitelist_id, users : address * whitelist_id_t option * users_t) : users_t =
  Big_map.update user whitelist_id users

(* -- | Specialized `get` *)
(* userWhitelist :: forall a s. IsComparable a => a & Users a & s :-> Maybe WhitelistId & s *)
let get_user_whitelist (user, users : address * users_t) : (whitelist_id_t option) =
  Big_map.find_opt user users

(* -- | Assert that the user is on a whitelist *)
(* assertUserWhitelist :: IsComparable a => a & Users a & s :-> WhitelistId & s *)
let assert_user_whitelist (user, users : address * users_t) : whitelist_id_t =
  match get_user_whitelist (user, users) with
    None -> (failwith "User not on a whitelist" : whitelist_id_t)
  | Some (whitelist_id) -> whitelist_id

(* -- | Assert that the users are on whitelists *)
(* assertUsersWhitelist :: IsComparable a *)
(*   => a & a & Users a & s :-> WhitelistId & WhitelistId & s *)
let assert_users_whitelist (user_x, user_y, users : address * address * users_t) : (whitelist_id_t * whitelist_id_t) =
  (assert_user_whitelist(user_x, users), assert_user_whitelist(user_y, users))

(* -- | Specialized `update` *)
(* setOutboundWhitelists :: forall s. WhitelistId & Maybe OutboundWhitelists & Whitelists & s :-> Whitelists & s *)
let set_outbound_whitelists (whitelist_id, outbound_whitelists_option, whitelists : whitelist_id_t * outbound_whitelists_t option * whitelists_t) : whitelists_t =
  Big_map.update whitelist_id outbound_whitelists_option whitelists

(* -- | Specialized `get` *)
(* outboundWhitelists :: forall s. WhitelistId & Whitelists & s :-> Maybe OutboundWhitelists & s *)
let get_outbound_whitelists (whitelist_id, whitelists : whitelist_id_t * whitelists_t) : (outbound_whitelists_t option) =
  Big_map.find_opt whitelist_id whitelists

(* -- | Assert that a `WhitelistId` has associated `OutboundWhitelists` *)
(* assertOutboundWhitelists :: WhitelistId & Whitelists & s :-> OutboundWhitelists & s *)
let assert_outbound_whitelists (whitelist_id, whitelists : whitelist_id_t * whitelists_t) : outbound_whitelists_t =
  match get_outbound_whitelists (whitelist_id, whitelists) with
    None -> (failwith "Whitelist does not exist" : outbound_whitelists_t)
  | Some (outbound_whitelists) -> outbound_whitelists

(* -- | Assert that `OutboundWhitelists` `restricted` is `False` *)
(* assertUnrestrictedOutboundWhitelists :: OutboundWhitelists & s :-> Set WhitelistId & s *)
let assert_unrestricted_outbound_whitelists (outbound_whitelists : outbound_whitelists_t) : (whitelist_id_t set) =
  if outbound_whitelists.unrestricted then
    outbound_whitelists.allowed_whitelists
  else
    (failwith "outbound restricted" : whitelist_id_t set)


(* ----------- *)
(* Entrypoints *)
(* ----------- *)

(* -- | Assert that one user is allowed to transfer to the other *)
(* -- assertTransfer :: forall a. (IsComparable a, CompareOpHs a, Typeable a) => Entrypoint (TransferParams a) (Storage a) *)
(* assertTransfer :: forall a s. (IsComparable a, CompareOpHs a, Typeable a) => TransferParams a & Storage a & s :-> ([Operation], Storage a) & s *)
let assert_transfer (transfer_params, storage : transfer_params_t * storage_t) : return_t =
  let new_storage: storage_t =
    if (transfer_params.from = storage.issuer) then
      storage
    else
      let from_to_whitelists: (whitelist_id_t * whitelist_id_t) =
        assert_users_whitelist(transfer_params.from, transfer_params.to_, storage.users) in
      let from_whitelist_id: whitelist_id_t = from_to_whitelists.0 in
      let to_whitelist_id: whitelist_id_t = from_to_whitelists.1 in
      let from_outbound_whitelists: outbound_whitelists_t =
        assert_outbound_whitelists(from_whitelist_id, storage.whitelists) in
      let from_allowed_whitelist_ids: whitelist_id_t set =
        assert_unrestricted_outbound_whitelists(from_outbound_whitelists) in
      if Set.mem to_whitelist_id from_allowed_whitelist_ids then
        storage
      else
        (failwith "outbound not whitelisted" : storage_t)
  in (no_op, new_storage)

(* -- | Set the issuer *)
(* -- *)
(* -- Only admin *)
(* setIssuer :: forall a. () => Entrypoint a (Storage a) *)
let set_issuer (new_issuer, storage : address * storage_t) : return_t =
  let admin_storage: storage_t = assert_admin(storage) in
  (no_op, { admin_storage with issuer = new_issuer})

(* -- | Assert not equal with an error: @"issuer is not a user"@ *)
(* assertNotIssuer :: (CompareOpHs a, Typeable a) => a & a & s :-> a & a & s *)
let assert_not_issuer(user, storage : address * storage_t) : address =
  if (user = storage.issuer) then
    (failwith "issuer is not a user" : address)
  else
    user

(* -- | Add a user with a particular `WhitelistId`, *)
(* -- or implicitly remove by providing `Nothing` *)
(* -- *)
(* -- Only admin *)
(* addUser :: forall a. (CompareOpHs a, Typeable a) => Entrypoint (NewUserParams a) (Storage a) *)
let add_user (new_user_params, storage : new_user_params_t * storage_t) : return_t =
  let admin_storage: storage_t = assert_admin(storage) in
  let new_user: address = assert_not_issuer(new_user_params.new_user, admin_storage) in
  let new_users: users_t = add_user_whitelist(new_user, new_user_params.new_user_whitelist, admin_storage.users) in
  (no_op, { admin_storage with users = new_users })

(* -- | Set the `WhitelistOutboundParams` for a `WhitelistId` *)
(* -- *)
(* -- Only admin *)
(* setWhitelistOutbound :: forall a. () => Entrypoint WhitelistOutboundParams (Storage a) *)
let set_whitelist_outbound (whitelist_outbound_params, storage : whitelist_outbound_params_t * storage_t) : return_t =
  let admin_storage: storage_t = assert_admin(storage) in
  let new_whitelists: whitelists_t = set_outbound_whitelists(whitelist_outbound_params.whitelist_id, whitelist_outbound_params.new_outbound_whitelists, admin_storage.whitelists) in
  (no_op, { admin_storage with whitelists = new_whitelists })

(* -- | Set the admin `Address` *)
(* -- *)
(* -- Only admin *)
(* setAdmin :: forall a. () => Entrypoint Address (Storage a) *)
let set_admin (new_admin, storage : address * storage_t) : return_t =
  let admin_storage: storage_t = assert_admin(storage) in
  (no_op, { admin_storage with admin = new_admin })


(* ------------------ *)
(* -- View parameters *)
(* ------------------ *)

(* -- | Get the issuer, who may be set by the admin and is unrestricted *)
(* getIssuer :: forall a. (NiceParameter a) => Entrypoint (View_ a) (Storage a) *)
let get_issuer (view, storage : (unit * address contract) * storage_t) : return_t =
  let op : operation = Tezos.transaction storage.issuer 0mutez view.1 in
  [op], storage

(* -- | Get a user's `WhitelistId`, or `Nothing` if the user is not present *)
(* getUser :: forall a. (IsComparable a) => Entrypoint (View a (Maybe WhitelistId)) (Storage a) *)
let get_user (view, storage : (address * whitelist_id_t option contract) * storage_t) : return_t =
  let op : operation = Tezos.transaction (get_user_whitelist(view.0, storage.users)) 0mutez view.1 in
  [op], storage

(* -- | Get the `OutboundWhitelists` of a `WhitelistId` or `Nothing` if it's not present *)
(* getWhitelist :: forall a. () => Entrypoint (View WhitelistId (Maybe OutboundWhitelists)) (Storage a) *)
let get_whitelist (view, storage : (whitelist_id_t * outbound_whitelists_t option contract) * storage_t) : return_t =
  let op : operation = Tezos.transaction (get_outbound_whitelists(view.0, storage.whitelists)) 0mutez view.1 in
  [op], storage

(* -- | Get the admin `Address` of the contract *)
(* getAdmin :: forall a. () => Entrypoint (View_ Address) (Storage a) *)
let get_admin (view, storage : (unit * address contract) * storage_t) : return_t =
  let op : operation = Tezos.transaction storage.admin 0mutez view.1 in
  [op], storage

let whitelist_contract_other_params (other_params, storage : other_params_t * storage_t) : return_t =
  match other_params with
    SetIssuer (new_issuer) -> set_issuer(new_issuer, storage)
  | AddUser (new_user) -> add_user(new_user, storage)
  | SetWhitelistOutbound (whitelist_outbound_params) -> set_whitelist_outbound(whitelist_outbound_params, storage)
  | SetAdmin (new_admin) -> set_admin(new_admin, storage)
  | GetIssuer (view) -> get_issuer(view, storage)
  | GetUser (view) -> get_user(view, storage)
  | GetWhitelist (view) -> get_whitelist(view, storage)
  | GetAdmin (view) -> get_admin(view, storage)

(* whitelistContract :: forall a. (IsComparable a, CompareOpHs a, Typeable a, KnownValue a, NoOperation a) *)
(*   => Contract (Parameter a) (Storage a) *)
let whitelist_contract (parameter, storage : parameter_t * storage_t) : return_t =
  match parameter with
    AssertTransfer (transfer_params) -> assert_transfer(transfer_params, storage)
  | OtherParameter (other_params) -> whitelist_contract_other_params(other_params, storage)

