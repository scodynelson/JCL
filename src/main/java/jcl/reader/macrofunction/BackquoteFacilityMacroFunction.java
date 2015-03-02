/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.packages.GlobalPackageStruct;
import jcl.system.EnhancedLinkedList;

abstract class BackquoteFacilityMacroFunction extends ReaderMacroFunctionImpl {

	protected static final SymbolElement APPEND = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "APPEND");

	protected static final SymbolElement CONS = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "CONS");

	protected static final SymbolElement LIST = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "LIST");

	protected static final SymbolElement LIST_STAR = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "LIST*");

	protected static final SymbolElement NCONC = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "NCONC");

	protected static final SymbolElement NIL = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "NIL");

	protected static final SymbolElement QUOTE = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "QUOTE");

	protected static final SymbolElement T = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "T");

	protected static final SymbolElement BQ_COMMA_FLAG = new SymbolElement(GlobalPackageStruct.BACKQUOTE.getName(), ",");

	protected static final SymbolElement BQ_AT_FLAG = new SymbolElement(GlobalPackageStruct.BACKQUOTE.getName(), ",@");

	protected static final SymbolElement BQ_DOT_FLAG = new SymbolElement(GlobalPackageStruct.BACKQUOTE.getName(), ",.");

	private static final long serialVersionUID = 816197148338656390L;

	protected static ConsElement getConsElement(final SimpleElement flag, final SimpleElement code) {
		final ConsElement consElement;
		if (code instanceof NullElement) {
			consElement = new ConsElement(flag);
		} else if (code instanceof ConsElement) {
			final ConsElement codeConsElement = (ConsElement) code;
			final EnhancedLinkedList<SimpleElement> codeElements = codeConsElement.getElements();

			final EnhancedLinkedList<SimpleElement> bqReturnThingElements = new EnhancedLinkedList<>();
			bqReturnThingElements.add(flag);
			bqReturnThingElements.addAll(codeElements);

			final boolean isDotted = codeConsElement.isDotted();
			consElement = new ConsElement(isDotted, bqReturnThingElements);
		} else {
			// This is fine to create ConsElement like this here since the second element is not a ListElement type
			consElement = new ConsElement(true, flag, code);
		}
		return consElement;
	}

	protected static SimpleElement getCdrElement(final EnhancedLinkedList<SimpleElement> elements, final boolean isDotted) {
		if (elements.isEmpty()) {
			return NullElement.INSTANCE;
		} else if (isDotted && (elements.size() == 1)) {
			return elements.getFirst();
		} else {
			return new ConsElement(isDotted, elements);
		}
	}
}
