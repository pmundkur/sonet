open TLSConstants
open Bytes

type sessionID = string
type role =
| Client
| Server

(* Client/Server randomness *)
type random = string
type crand = random
type srand = random

type pmsData =
| PMSUnset
| RSAPMS of RSAKey.pk * protocolVersion * bytes
| DHPMS  of DHGroup.p * DHGroup.g * DHGroup.elt * DHGroup.elt

type sessionInfo = {
  init_crand: crand;
  init_srand: srand;
  protocol_version: protocolVersion;
  cipher_suite: cipherSuite;
  compression: compression;
  pmsData: pmsData;
  client_auth: bool;
  clientID: Cert.cert list;
  serverID: Cert.cert list;
  sessionID: sessionID;
  (* Extensions: *)
  extended_record_padding: bool;
}

type epoch

val isInitEpoch: epoch -> bool
val epochSI: epoch -> sessionInfo
val epochSRand: epoch -> srand
val epochCRand: epoch -> crand

(* Role is of the writer *)
type connectionInfo = {
  role: role;
  id_rand: random;
  id_in:  epoch;
  id_out: epoch
}
val connectionRole: connectionInfo -> role

val initConnection: role -> bytes -> connectionInfo
val nextEpoch: epoch -> crand -> srand -> sessionInfo -> epoch

(* Application configuration options *)

type helloReqPolicy =
| HRPIgnore
| HRPFull
| HRPResume

type config = {
  minVer: protocolVersion;
  maxVer: protocolVersion;
  ciphersuites: cipherSuites;
  compressions: compression list;

  (* Handshake specific options *)

  (* Client side *)
  honourHelloReq: helloReqPolicy;
  allowAnonCipherSuite: bool;

  (* Server side *)
  request_client_certificate: bool;
  check_client_version_in_pms_for_old_tls: bool;

  (* Common *)
  safe_renegotiation: bool;
  server_name: Cert.hint;
  client_name: Cert.hint;

  (* Sessions database *)
  sessionDBFileName: string;
  sessionDBExpiry: timeSpan;
}

val defaultConfig: config

val max_TLSCipher_fragment_length: nat
val fragmentLength: nat
