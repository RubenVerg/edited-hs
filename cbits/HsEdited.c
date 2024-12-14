#include "HsEdited.h"

#include <stdio.h>

Edited *edited_init_hs(const char *program, int in_fd, int out_fd, int err_fd) {
	FILE* in = fdopen(in_fd, "r");
	FILE* out = fdopen(out_fd, "w");
	FILE* err = fdopen(err_fd, "w");
	Edited* el = edited_init_fd(program, in, out, err, in_fd, out_fd, err_fd);
	return el;
}

bool edited_set_editor(Edited *el, const char *mode) {
	return edited_set(el, EL_EDITOR, mode) != 0;
}

bool edited_set_prompt(Edited *el, wchar_t* (*f)(Edited*)) {
	return edited_wset(el, EL_PROMPT, f) != 0;
}

bool edited_set_rprompt(Edited *el, wchar_t* (*f)(Edited*)) {
	return edited_wset(el, EL_RPROMPT, f) != 0;
}

bool edited_set_addfn(Edited *el, const char *name, const char *desc, int (*f)(Edited*, wint_t)) {
	return edited_set(el, EL_ADDFN, name, desc, f);
}

bool edited_set_bind(Edited *el, const char* keybind, const char* fnName) {
	return edited_set(el, EL_BIND, keybind, fnName, NULL) != 0;
}

bool edited_set_use_style(Edited *el, bool use) {
	return edited_wset(el, EL_USE_STYLE, use) != 0;
}

bool edited_set_style_func(Edited *el, void (*f)(Edited*, int, const wchar_t*, edited_style_t*)) {
	return edited_wset(el, EL_STYLE_FUNC, f) != 0;
}
