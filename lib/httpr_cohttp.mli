include Httpr_intf.S

module Lwt : sig
  module Response = Httpr_intf.Response

  val get :
    ?timeout:int ->
    ?verbose:bool ->
    ?redirects:int ->
    ?headers:string list ->
    Uri.t ->
    (Response.t, string) result Lwt.t

  val gets :
    ?timeout:int ->
    ?verbose:bool ->
    ?redirects:int ->
    ?headers:string list ->
    Uri.t list ->
    (Response.t, string) result list Lwt.t

  val gets_keyed :
    ?timeout:int ->
    ?verbose:bool ->
    ?redirects:int ->
    ?headers:string list ->
    Uri.t list ->
    (Response.t, string) result list Lwt.t
end
