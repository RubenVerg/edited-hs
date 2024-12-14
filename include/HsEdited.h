#ifndef _hs_edited_h
#define _hs_edited_h

#include <stdbool.h>

#include "config.h"
#include "edited/edited.h"
#include "edited/style.h"

extern Edited* edited_init_hs(const char* program, int in_fd, int out_fd, int err_fd);

extern bool edited_set_editor(Edited* el, const char* mode);
extern bool edited_set_prompt(Edited* el, wchar_t* (*f)(Edited*));
extern bool edited_set_rprompt(Edited* el, wchar_t* (*f)(Edited*));
extern bool edited_set_addfn(Edited* el, const char* name, const char* desc, int (*f)(Edited*, wint_t));
extern bool edited_set_bind(Edited* el, const char* keybind, const char* fnName);
extern bool edited_set_use_style(Edited* el, bool use);
extern bool edited_set_style_func(Edited* el, void (*f)(Edited*, int, const wchar_t*, edited_style_t*));

#endif