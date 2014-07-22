package jcl.printer;

import jcl.LispStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

import java.util.List;

public class ListStructPrinter {

	public static String print(final ListStruct listStruct) {

		if (listStruct instanceof NullStruct) {
			return "NIL";
		} else if (listStruct instanceof ConsStruct) {
			final ConsStruct consStruct = (ConsStruct) listStruct;

			if (consStruct.isCircular()) {
				return "CIRCULAR LIST PRINTING NOT YET SUPPORTED!!!";
			}

			final List<LispStruct> javaList = consStruct.getAsJavaList();
			final StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.append('(');
			for (int i = 0; i < javaList.size(); i++) {
				if (i > 0) {
					stringBuilder.append(' ');
				}

				if (consStruct.isDotted() && ((i + 1) == javaList.size())) {
					stringBuilder.append(". ");
				}
				stringBuilder.append(Printer.print(javaList.get(i)));
			}
			stringBuilder.append(')');

			return stringBuilder.toString();
		} else {
			return listStruct.toString();
		}
	}
}
