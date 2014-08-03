package jcl.compiler.old.functions;

import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;

public final class NReverseFunction {

	private NReverseFunction() {
	}

	public static ListStruct funcall(final ListStruct arg1) {

		if (arg1.equals(NullStruct.INSTANCE)) {
			return arg1;
		}

		ListStruct previousCons = NullStruct.INSTANCE;

		ConsStruct cons = (ConsStruct) arg1;
		ListStruct nextCons = arg1.getRest();

		while (!nextCons.equals(NullStruct.INSTANCE)) {
			cons.setCdr(previousCons);

			previousCons = cons;
			cons = (ConsStruct) nextCons;
			nextCons = nextCons.getRest();
		}

		cons.setCdr(previousCons);
		return cons;
	}
}
