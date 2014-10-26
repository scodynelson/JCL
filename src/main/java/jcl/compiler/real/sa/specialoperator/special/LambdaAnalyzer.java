package jcl.compiler.real.sa.specialoperator.special;

import jcl.LispStruct;
import jcl.compiler.old.functions.AppendFunction;
import jcl.compiler.old.functions.AssocFunction;
import jcl.compiler.old.functions.GensymFunction;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.LambdaEnvironmentListStruct;
import jcl.compiler.real.sa.LambdaListParser;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.Declaration;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.Scanner;
import java.util.regex.Pattern;

public class LambdaAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LambdaAnalyzer INSTANCE = new LambdaAnalyzer();

	private static String nameBreakingRegex = "[^\\p{Alnum}]";
	private static Pattern nameBreakingPattern = Pattern.compile(nameBreakingRegex);

	@Override
	public ListStruct analyze(final ListStruct input) {

		if (input.size() < 2) {
			throw new RuntimeException("Wrong number of arguments to special operator Lambda: " + input.size());
		}

		final Environment environment = SemanticAnalyzer.environmentStack.peek();

		final LispStruct secondElement = input.getRest().getFirst();
		if (!(secondElement instanceof ListStruct)) {
			throw new RuntimeException("Second argument to Lambda must be a ListStruct of parameters");
		}

		final ListStruct parameters = (ListStruct) secondElement;
		final OrdinaryLambdaListBindings parsedLambdaList = LambdaListParser.parseOrdinaryLambdaList(parameters);
/* TODO
		// Add the lambda bindings to the current environment
		final ConsStruct bindingList = (ConsStruct) EnvironmentAccessor.getBindingSet(environment);
		// Now I have to remove any FAKE rest forms. They can't be in the binding set
		bindingList.setCdr(parsedLambdaList); // NOT AUX FORMS?!?!
*/
		final ListStruct declaresDocStringBody = input.getRest().getRest();
		final ListStruct orderedDeclaresDocStringBody = saDeclarations(declaresDocStringBody);

		// TODO: the old code did this whole "ArgInitFormUsage" stuff. It basically does the work that determines
		// TODO: if the 'supplied-p' parameter has a values of T or NIL. What it will do is 'inline' an "if" block that
		// TODO: will dynamically check the input of the variable and then "SETQ" the value of the 'supplied-p' parameter
		// TODO: to the 'init-form' if not supplied.

		// TODO: Next section is basically creating a bunch of (lambda (setq ,var ,var-init-val)) if the var is a :optional or :key
		// BLAH BLAH, SETQ STUFF GOES HERE

/* TODO
		// the current min for binding is 1 since it's a lambda
		SemanticAnalyzer.bindingsPosition = 0;

		// there may be &aux Variables that get turned into a let* that starts before the
		// block construct.
		if ((auxValues != null) && !auxValues.equals(NullStruct.INSTANCE)) {
			afterDecls = new ConsStruct(SpecialOperator.LET_STAR, new ConsStruct(auxValues, afterDecls));
			afterDecls = new ConsStruct(afterDecls, NullStruct.INSTANCE);
		}

		// now splice the static-field for as the first line of the body
		((ConsStruct) declsOnward).setCdr(afterDecls);
*/
/*
		// Now deal with analyzing the BODY

		List<LispStruct> bodyElements = list.getRest().getAsJavaList();
		List<LispStruct> newBodyElements = new ArrayList<>();
		for (final LispStruct element : bodyElements) {
			newBodyElements.add(SemanticAnalyzer.saMainLoop(element));
		}
*/
		// TODO: set the current environment back to what it was before we hit this method
		return new LambdaEnvironmentListStruct(environment, parsedLambdaList, null);
	}

	/**
	 * This method parses the declaration specifications - which may also
	 * hold a documentation string. All of the declarations are merged into
	 * a single DECLARE form. The doc string becomes another DECLARE option.
	 * The BNF is
	 * declarations := declSpec* docString? declSpec*
	 * declSpec := '(' DECLARE declaration* ')'
	 * declaration := '(' declName declArgs ')'
	 * <p>
	 * If there is nothing after the string, then the string is the form
	 * Not the doc string
	 */
	public static ListStruct saDeclarations(ListStruct list) {
		// list is the rest of the forms after the arg list
		ListStruct newDecls = NullStruct.INSTANCE;   //the list of all new Declarations
		final ListStruct returnList = list;

		while (!list.equals(NullStruct.INSTANCE)) {
			LispStruct theCar = list.getFirst();
			ListStruct theDecl;    //the current new Declaration
			if (theCar instanceof CharSequence) {
				// This may be a doc string, unless it's the last thing
				if (list.getRest().equals(NullStruct.INSTANCE)) {
					break;
				} else {
					// there's stuff after the doc string so it is doc
					theDecl = ListStruct.buildProperList(ListStruct.buildProperList(Declaration.DOCUMENTATION, theCar));
					newDecls = (ListStruct) AppendFunction.funcall(newDecls, theDecl);
					list = list.getRest();

					// add code to look for more declarations - without any strings
					theCar = list.getFirst();
					while ((theCar instanceof ListStruct)
							&& ((ListStruct) theCar).getFirst().equals(SpecialOperator.DECLARE)) {
						theDecl = ((ListStruct) theCar).getRest();
						newDecls = (ListStruct) AppendFunction.funcall(newDecls, theDecl);
						list = list.getRest();
						theCar = list.getFirst();
					}
					// saDeclarationsHelp will complete the search for declarations
					break;
				}
			}
			if ((theCar instanceof ListStruct) && ((ListStruct) theCar).getFirst().equals(SpecialOperator.DECLARE)) {
				theDecl = ((ListStruct) theCar).getRest();
				newDecls = (ListStruct) AppendFunction.funcall(newDecls, theDecl);
			} else {
				break;
			}
			list = list.getRest();
		}

		// if there isn't a Declaration.JAVA_CLASS_NAME, add one
		final ListStruct classNameDecl = AssocFunction.funcall(Declaration.JAVA_CLASS_NAME, newDecls);
		if (classNameDecl.equals(NullStruct.INSTANCE)) {
			final SymbolStruct<?> classname;
			// if there's a Lisp Name, then Javafy it
			final ListStruct lispNameDecl = AssocFunction.funcall(Declaration.LISP_NAME, newDecls);
			if (lispNameDecl.equals(NullStruct.INSTANCE)) {
				SemanticAnalyzer.currentLispName.push(null);
				final String name = "AnonymousLambda_" + System.currentTimeMillis() + '_';
				classname = (SymbolStruct) GensymFunction.funcall(name);
			} else {
				final SymbolStruct<?> lispName = (SymbolStruct) lispNameDecl.getRest().getFirst();
				classname = (SymbolStruct) GensymFunction.funcall(javafy(lispName) + System.currentTimeMillis());
				// this code allows saFunctionCall to recognize a recursive call
				// the pop happens at the end of saLambda
				SemanticAnalyzer.currentLispName.push(lispName);
			}
			newDecls = new ConsStruct(new ConsStruct(Declaration.JAVA_CLASS_NAME, ListStruct.buildProperList(classname)), newDecls);
		}
		SemanticAnalyzer.currentLispName.push(null);
		// if the value is a String, make it into a SymbolStruct
		final Object name = classNameDecl.getRest().getFirst();
		if (name instanceof CharSequence) {
			final ListStruct rest = classNameDecl.getRest();
			rest.setElement(1, new SymbolStruct<>(name.toString()));
		}
		// now we conjure a new declaration
		return new ConsStruct(new ConsStruct(SpecialOperator.DECLARE, newDecls), list);
	}

	// routine to javafy a Lisp SymbolStruct or string to a Java identifier
	private static String javafy(final Object lispName) {
		final StringBuilder result = new StringBuilder();
		final Scanner scanner = new Scanner(lispName.toString()).useDelimiter(nameBreakingPattern);

		while (scanner.hasNext()) {
			String fragment = scanner.next();
			if ((fragment == null) || fragment.isEmpty()) {
				fragment = "__";
			}
			final int codePoint = Character.toUpperCase(fragment.codePointAt(0));
			result.appendCodePoint(codePoint);
			result.append(fragment.substring(1).toLowerCase());
		}
		if (result.length() == 0) {
			result.append("UnknownLispName");
		}
		return result.toString();
	}
}
