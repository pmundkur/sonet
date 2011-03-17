LIB = sonet

EVENTLOOP_DIR = eventloop
HTTP_DIR = protocols/http
JSON_DIR = protocols/jsonrpc
DBUS_DIR = protocols/dbus

EVENTLOOP_OBJ_DIR = _build/$(EVENTLOOP_DIR)
HTTP_OBJ_DIR = _build/$(HTTP_DIR)
JSON_OBJ_DIR = _build/$(JSON_DIR)
DBUS_OBJ_DIR = _build/$(DBUS_DIR)

EVENTLOOP_INSTALLS = \
	$(EVENTLOOP_OBJ_DIR)/eventloop.{a,cma,cmxa} \
	$(EVENTLOOP_OBJ_DIR)/{dlleventloop.so,libeventloop.a} \
	$(EVENTLOOP_OBJ_DIR)/{async_conn,eventloop}.{cmi,mli}
HTTP_INSTALLS = \
	$(HTTP_OBJ_DIR)/httplib.{a,cma,cmxa} \
	$(HTTP_OBJ_DIR)/http{,_client,_client_conn}.{cmi,mli} \
	$(HTTP_OBJ_DIR)/uri.{cmi,mli}
JSON_INSTALLS = \
	$(JSON_OBJ_DIR)/jsonlib.{a,cma,cmxa} \
	$(JSON_OBJ_DIR)/json{,_conv,_parse,rpc}.{cmi,mli}
DBUS_INSTALLS = \
	$(DBUS_OBJ_DIR)/dbuslib.{a,cma,cmxa} \
	$(DBUS_OBJ_DIR)/{dllodbus.so,libodbus.a} \
	$(DBUS_OBJ_DIR)/dbus_*.{cmi,mli}

INSTALLS = $(EVENTLOOP_INSTALLS) $(HTTP_INSTALLS) $(JSON_INSTALLS) $(DBUS_INSTALLS)

.PHONY: all clean install uninstall reinstall install_test

all:
	ocamlbuild all.otarget

clean:
	ocamlbuild -clean
	make -C $(HTTP_DIR)/tests clean
	make -C $(JSON_DIR)/tests clean
	make -C $(DBUS_DIR)/tests  clean

install: all
	ocamlfind install $(LIB) META $(INSTALLS)

uninstall:
	ocamlfind remove $(LIB)

reinstall: uninstall install

install_test:
	make -C $(HTTP_DIR)/tests
	make -C $(JSON_DIR)/tests
	make -C $(DBUS_DIR)/tests
