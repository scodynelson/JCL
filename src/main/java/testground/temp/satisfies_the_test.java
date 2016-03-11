package testground.temp;

public final class satisfies_the_test extends CompiledPrimitive {

	public LispObject execute(final LispObject lispobject, final LispObject lispobject1) {
		final LispThread lispthread = LispThread.currentThread();

		final LispObject objectCdr = lispobject.cdr();

		final LispObject execute = lispthread.execute(SYM19374, objectCdr);
		if (((Fixnum) execute).value != 2) {
			lispthread.execute(SYM19377, SYM19378, SYM19379, objectCdr, OBJ19380, INT19381, INT19381);
		}
		final LispObject lispobject2 = lispobject.cadr();
		final LispObject lispobject3 = objectCdr.cadr();

		final Symbol symbol = Lisp.gensym(lispthread);

		return new Cons(SYM19398,
		                new Cons(new Cons(new Cons(symbol,
		                                           new Cons(new Cons(SYM19399,
		                                                             new Cons(SYM19400,
		                                                                      new Cons(lispobject3)))))),
		                         new Cons(new Cons(SYM19401,
		                                           new Cons(new Cons(SYM19402,
		                                                             new Cons(new Cons(SYM19403,
		                                                                               new Cons(SYM19404,
		                                                                                        new Cons(lispobject2,
		                                                                                                 new Cons(symbol)))))),
		                                                    new Cons(new Cons(SYM19405,
		                                                                      new Cons(new Cons(SYM19406,
		                                                                                        new Cons(new Cons(SYM19403,
		                                                                                                          new Cons(SYM19407,
		                                                                                                                   new Cons(lispobject2,
		                                                                                                                            new Cons(symbol)))))))),
		                                                             new Cons(new Cons(SYM19408,
		                                                                               new Cons(new Cons(SYM19403,
		                                                                                                 new Cons(SYM19404,
		                                                                                                          new Cons(lispobject2,
		                                                                                                                   new Cons(symbol)))))))))))));
	}

	public satisfies_the_test() {
		super(Lisp.NIL, Lisp.readObjectFromString("(#0? #1?)"));
	}

	static final Symbol SYM19408 = (Symbol) Lisp.recall("SYM19408");
	static final Symbol SYM19407 = (Symbol) Lisp.recall("SYM19407");
	static final Symbol SYM19406 = (Symbol) Lisp.recall("SYM19406");
	static final Symbol SYM19405 = (Symbol) Lisp.recall("SYM19405");
	static final Symbol SYM19404 = (Symbol) Lisp.recall("SYM19404");
	static final Symbol SYM19403 = (Symbol) Lisp.recall("SYM19403");
	static final Symbol SYM19402 = (Symbol) Lisp.recall("SYM19402");
	static final Symbol SYM19401 = (Symbol) Lisp.recall("SYM19401");
	static final Symbol SYM19400 = (Symbol) Lisp.recall("SYM19400");
	static final Symbol SYM19399 = (Symbol) Lisp.recall("SYM19399");
	static final Symbol SYM19398 = (Symbol) Lisp.recall("SYM19398");
	static final LispInteger INT19381 = (LispInteger) Lisp.recall("INT19381");
	static final LispObject OBJ19380 = Lisp.recall("OBJ19380");
	static final Symbol SYM19379 = (Symbol) Lisp.recall("SYM19379");
	static final Symbol SYM19378 = (Symbol) Lisp.recall("SYM19378");
	static final Symbol SYM19377 = (Symbol) Lisp.recall("SYM19377");
	static final Symbol SYM19374 = (Symbol) Lisp.recall("SYM19374");
}

/*
(sym19398 ((symbol (sym19399 sym19400 lispobject3)))
  (sym19401 (sym19402 (sym19403 sym19404 lispobject2 symbol))
    (sym19405 (sym19406 (sym19403 sym19407 lispobject2 symbol))
			  (sym19408 (sym19403 sym19404 lispobject2 symbol)))))


(defmacro satisfies-the-test (item elt)
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key ,elt)))
      (cond (test-p (funcall test ,item ,key-tmp))
	        (test-not-p (not (funcall test-not ,item ,key-tmp)))
	        (t (funcall test ,item ,key-tmp))))))

 */