LIB = sonet

EVENTLOOP_DIR = eventloop
HTTP_DIR = protocols/http
JSON_DIR = protocols/jsonrpc
DBUS_DIR = protocols/dbus
BENCODE_DIR = protocols/bencode

EVENTLOOP_OBJ_DIR = _build/$(EVENTLOOP_DIR)
HTTP_OBJ_DIR = _build/$(HTTP_DIR)
JSON_OBJ_DIR = _build/$(JSON_DIR)
DBUS_OBJ_DIR = _build/$(DBUS_DIR)
BENCODE_OBJ_DIR = _build/$(BENCODE_DIR)

EVENTLOOP_INSTALLS = \
	$(EVENTLOOP_OBJ_DIR)/eventloop.a $(EVENTLOOP_OBJ_DIR)/eventloop.cma $(EVENTLOOP_OBJ_DIR)/eventloop.cmxa \
	$(EVENTLOOP_OBJ_DIR)/dlleventloop.so $(EVENTLOOP_OBJ_DIR)/libeventloop.a \
	$(EVENTLOOP_OBJ_DIR)/async_conn.mli $(EVENTLOOP_OBJ_DIR)/async_conn.cmi \
	$(EVENTLOOP_OBJ_DIR)/eventloop.mli $(EVENTLOOP_OBJ_DIR)/eventloop.cmi
HTTP_INSTALLS = \
	$(HTTP_OBJ_DIR)/httplib.a $(HTTP_OBJ_DIR)/httplib.cma $(HTTP_OBJ_DIR)/httplib.cmxa \
	$(HTTP_OBJ_DIR)/http.mli $(HTTP_OBJ_DIR)/http.cmi \
	$(HTTP_OBJ_DIR)/http_client.mli $(HTTP_OBJ_DIR)/http_client.cmi \
	$(HTTP_OBJ_DIR)/http_client_conn.mli $(HTTP_OBJ_DIR)/http_client_conn.cmi \
	$(HTTP_OBJ_DIR)/uri.mli $(HTTP_OBJ_DIR)/uri.cmi
JSON_INSTALLS = \
	$(JSON_OBJ_DIR)/jsonlib.a $(JSON_OBJ_DIR)/jsonlib.cma $(JSON_OBJ_DIR)/jsonlib.cmxa \
	$(JSON_OBJ_DIR)/json.mli $(JSON_OBJ_DIR)/json.cmi \
	$(JSON_OBJ_DIR)/json_conv.mli $(JSON_OBJ_DIR)/json_conv.cmi \
	$(JSON_OBJ_DIR)/json_parse.mli $(JSON_OBJ_DIR)/json_parse.cmi \
	$(JSON_OBJ_DIR)/jsonrpc.mli $(JSON_OBJ_DIR)/jsonrpc.cmi
DBUS_INSTALLS = \
	$(DBUS_OBJ_DIR)/dbuslib.a $(DBUS_OBJ_DIR)/dbuslib.cma $(DBUS_OBJ_DIR)/dbuslib.cmxa \
	$(DBUS_OBJ_DIR)/dllodbus.so $(DBUS_OBJ_DIR)/libodbus.a \
	$(DBUS_OBJ_DIR)/dbus_*.mli $(DBUS_OBJ_DIR)/dbus_*.cmi
BENCODE_INSTALLS =\
	$(BENCODE_OBJ_DIR)/bencodelib.a $(BENCODE_OBJ_DIR)/bencodelib.cma $(BENCODE_OBJ_DIR)/bencodelib.cmxa \
	$(BENCODE_OBJ_DIR)/bencode.mli $(BENCODE_OBJ_DIR)/bencode.cmi

INSTALLS = $(EVENTLOOP_INSTALLS) $(HTTP_INSTALLS) $(JSON_INSTALLS) $(DBUS_INSTALLS) $(BENCODE_INSTALLS)

.PHONY: all clean install uninstall reinstall install_test

all:
	ocamlbuild all.otarget

clean:
	ocamlbuild -clean
	make -C $(HTTP_DIR)/tests clean
	make -C $(JSON_DIR)/tests clean
	make -C $(DBUS_DIR)/tests clean
	make -C $(BENCODE_DIR)/tests clean

install:
	ocamlfind install $(LIB) META $(INSTALLS)

uninstall:
	ocamlfind remove $(LIB)

reinstall: uninstall install

install_test:
	make -C $(HTTP_DIR)/tests
	make -C $(JSON_DIR)/tests
	make -C $(DBUS_DIR)/tests
	make -C $(BENCODE_DIR)/tests
