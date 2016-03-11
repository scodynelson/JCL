package testground.temp;

public final class apply_key extends CompiledPrimitive {

	public LispObject execute(final LispObject lispobject, final LispObject lispobject1) {
		final LispThread lispthread = LispThread.currentThread();

		final LispObject objectCdr = lispobject.cdr();

		final LispObject execute = lispthread.execute(SYM19434, objectCdr);
		if (((Fixnum) execute).value != 2) {
			lispthread.execute(SYM19437, SYM19438, SYM19439, objectCdr, OBJ19440, INT19441, INT19441);
		}
		final LispObject lispobject2 = lispobject.cadr();
		final LispObject lispobject3 = objectCdr.cadr();

		return new Cons(SYM19452,
		                new Cons(lispobject2,
		                         new Cons(new Cons(SYM19453,
		                                           new Cons(lispobject2,
		                                                    new Cons(lispobject3))),
		                                  new Cons(lispobject3))));
	}

	public apply_key() {
		super(Lisp.NIL, Lisp.readObjectFromString("(#2? #3?)"));
	}

	static final Symbol SYM19453 = (Symbol) Lisp.recall("SYM19453");
	static final Symbol SYM19452 = (Symbol) Lisp.recall("SYM19452");
	static final LispInteger INT19441 = (LispInteger) Lisp.recall("INT19441");
	static final LispObject OBJ19440 = Lisp.recall("OBJ19440");
	static final Symbol SYM19439 = (Symbol) Lisp.recall("SYM19439");
	static final Symbol SYM19438 = (Symbol) Lisp.recall("SYM19438");
	static final Symbol SYM19437 = (Symbol) Lisp.recall("SYM19437");
	static final Symbol SYM19434 = (Symbol) Lisp.recall("SYM19434");

}

/*
(sym19452 (second lispobject)
  (sym19453 (second lispobject) (third lispobject))
  (third lispobject))



(defmacro apply-key (key element)
  `(if ,key
       (funcall ,key ,element)
     ,element))

 */