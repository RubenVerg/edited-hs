/*	$NetBSD: chared.h,v 1.30 2016/05/22 19:44:26 christos Exp $	*/

/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Christos Zoulas of Cornell University.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)chared.h	8.1 (Berkeley) 6/4/93
 */

/*
 * el.chared.h: Character editor interface
 */
#ifndef _h_chared
#define	_h_chared

/*
 * This is an issue of basic "vi" look-and-feel. Defining MOVE works
 * like real vi: i.e. the transition from command<->insert modes moves
 * the cursor.
 *
 * On the other hand we really don't want to move the cursor, because
 * all the editing commands don't include the character under the cursor.
 * Probably the best fix is to make all the editing commands aware of
 * this fact.
 */
#define	MOVE

/*
 * Undo information for vi - no undo in emacs (yet)
 */
typedef struct edited_c_undo_t {
	ssize_t	 len;			/* length of saved line */
	int	 cursor;		/* position of saved cursor */
	wchar_t	*buf;			/* full saved text */
} edited_c_undo_t;

/* redo for vi */
typedef struct edited_c_redo_t {
	wchar_t	*buf;			/* redo insert key sequence */
	wchar_t	*pos;
	wchar_t	*lim;
	edited_action_t	cmd;		/* command to redo */
	wchar_t	ch;			/* char that invoked it */
	int	count;
	int	action;			/* from edited_cv_action() */
} edited_c_redo_t;

/*
 * Current action information for vi
 */
typedef struct edited_c_vcmd_t {
	int	 action;
	wchar_t	*pos;
} edited_c_vcmd_t;

/*
 * Kill buffer for emacs
 */
typedef struct edited_c_kill_t {
	wchar_t	*buf;
	wchar_t	*last;
	wchar_t	*mark;
} edited_c_kill_t;

typedef void (*edited_zfunc_t)(Edited *, void *);
typedef const char *(*edited_afunc_t)(void *, const char *);

/*
 * Note that we use both data structures because the user can bind
 * commands from both editors!
 */
typedef struct edited_chared_t {
	edited_c_undo_t	edited_c_undo;
	edited_c_kill_t	edited_c_kill;
	edited_c_redo_t	edited_c_redo;
	edited_c_vcmd_t	edited_c_vcmd;
	edited_zfunc_t	edited_c_resizefun;
	edited_afunc_t	edited_c_aliasfun;
	void *		edited_c_resizearg;
	void *		edited_c_aliasarg;
} edited_chared_t;


#define	STRQQ		"\"\""

#define	isglob(a)	(strchr("*[]?", (a)) != NULL)

#define	NOP		0x00
#define	DELETE		0x01
#define	INSERT		0x02
#define	YANK		0x04

#define	CHAR_FWD	(+1)
#define	CHAR_BACK	(-1)

#define	MODE_INSERT	0
#define	MODE_REPLACE	1
#define	MODE_REPLACE_1	2


libedited_private int	 edited_cv__isword(wint_t);
libedited_private int	 edited_cv__isWord(wint_t);
libedited_private void	 edited_cv_delfini(Edited *);
libedited_private wchar_t *edited_cv__endword(wchar_t *, wchar_t *, int, int (*)(wint_t));
libedited_private int	 edited_ce__isword(wint_t);
libedited_private void	 edited_cv_undo(Edited *);
libedited_private void	 edited_cv_yank(Edited *, const wchar_t *, int);
libedited_private wchar_t *edited_cv_next_word(Edited*, wchar_t *, wchar_t *, int,
			int (*)(wint_t));
libedited_private wchar_t *edited_cv_prev_word(wchar_t *, wchar_t *, int, int (*)(wint_t));
libedited_private wchar_t *edited_c__next_word(wchar_t *, wchar_t *, int, int (*)(wint_t));
libedited_private wchar_t *edited_c__prev_word(wchar_t *, wchar_t *, int, int (*)(wint_t));
libedited_private void	 edited_c_insert(Edited *, int);
libedited_private void	 edited_c_delbefore(Edited *, int);
libedited_private void	 edited_c_delbefore1(Edited *);
libedited_private void	 edited_c_delafter(Edited *, int);
libedited_private void	 edited_c_delafter1(Edited *);
libedited_private int	 edited_c_gets(Edited *, wchar_t *, const wchar_t *);
libedited_private int	 edited_c_hpos(Edited *);

libedited_private int	 ch_init(Edited *);
libedited_private void	 ch_reset(Edited *);
libedited_private int	 ch_resizefun(Edited *, edited_zfunc_t, void *);
libedited_private int	 ch_aliasfun(Edited *, edited_afunc_t, void *);
libedited_private int	 ch_enlargebufs(Edited *, size_t);
libedited_private void	 ch_end(Edited *);

#endif /* _h_chared */
