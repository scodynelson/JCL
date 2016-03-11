package testground.temp;

public final class lambda_fn_call extends CompiledPrimitive {

	public final LispObject execute(final LispObject lispobject, final LispObject lispobject1,
	                                final LispObject lispobject2, final LispObject lispobject3,
	                                final LispObject lispobject4) {

		final LispThread lispthread = LispThread.currentThread();
		final Object obj;
		if (lispobject == Lisp.NIL) {
			obj = INT19550;
		} else {
			obj = lispthread.execute(lispobject, INT19550);
		}
		lispthread._values = null;
		if (lispobject3 != Lisp.NIL) {
			return lispthread.execute(lispobject1, INT19550, ((LispObject) (obj)));
		}
		if (lispobject4 == Lisp.NIL) {
			return lispthread.execute(lispobject1, INT19550, ((LispObject) (obj)));
		} else {
			lispthread._values = null;
			if (lispthread.execute(lispobject2, INT19550, ((LispObject) (obj))) == Lisp.NIL) {
				return Lisp.T;
			} else {
				return Lisp.NIL;
			}
		}
	}

	public lambda_fn_call() {
		super(Lisp.NIL, Lisp.readObjectFromString("(KEY TEST TEST-NOT TEST-P TEST-NOT-P)"));
	}

	static final LispInteger INT19550 = (LispInteger) Lisp.recall("INT19550");
}