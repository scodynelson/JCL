package jcl.compiler.sa.analyzer.declare;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.JavaClassNameDeclarationStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.DeclarationStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DeclareExpander extends MacroFunctionExpander<DeclareStruct> {

	@Autowired
	private Printer printer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.DECLARE;
	}

	@Override
	public DeclareStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // DECLARE SYMBOL

		final List<LispStruct> declSpecs = new ArrayList<>();
		iterator.forEachRemaining(declSpecs::add);

		final DeclareStruct declareElement = new DeclareStruct();

		for (final LispStruct declSpec : declSpecs) {
			if (!(declSpec instanceof ListStruct)) {
				final String printedObject = printer.print(declSpec);
				throw new TypeErrorException("DECLARE: DECLARATION-SPECIFIER must be a List. Got: " + printedObject);
			}
			final ListStruct declSpecList = (ListStruct) declSpec;

			final Iterator<LispStruct> declSpecIterator = declSpecList.iterator();
			final LispStruct declIdentifier = declSpecIterator.next();


			final List<LispStruct> declSpecBody = new ArrayList<>();
			declSpecIterator.forEachRemaining(declSpecBody::add);

			// now come the various cases
			if (declIdentifier.equals(DeclarationStruct.DECLARATION)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.DYNAMIC_EXTENT)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.FTYPE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.IGNORABLE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.IGNORE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.INLINE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.NOTINLINE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.OPTIMIZE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.JAVA_CLASS_NAME)) {
				final JavaClassNameDeclarationStruct jclds = saJavaClassNameDeclaration(declSpecBody);
				declareElement.setJavaClassNameDeclaration(jclds);
			} else if (declIdentifier.equals(DeclarationStruct.LISP_NAME)) {
				final JavaClassNameDeclarationStruct jclds = saLispNameDeclaration(declSpecBody);
				declareElement.setJavaClassNameDeclaration(jclds);
			} else if (declIdentifier.equals(DeclarationStruct.SPECIAL)) {
				final List<SpecialDeclarationStruct> sds = saSpecialDeclaration(declSpecBody);
				declareElement.getSpecialDeclarations().addAll(sds);
			} else if (declIdentifier.equals(DeclarationStruct.TYPE)) {
				//TODO: we don't do anything here yet
			} else {
				throw new ProgramErrorException("DECLARE: Declaration specifier not supported: " + declIdentifier);
			}
		}

		return declareElement;
	}

	private JavaClassNameDeclarationStruct saJavaClassNameDeclaration(final List<LispStruct> declSpecBody) {

		final int declSpecBodySize = declSpecBody.size();
		if (declSpecBodySize != 1) {
			throw new ProgramErrorException("DECLARE: Incorrect number of arguments for JAVA-CLASS-NAME declaration: " + declSpecBodySize + ". Expected 1 argument.");
		}

		final LispStruct javaClassName = declSpecBody.get(0);
		if (!(javaClassName instanceof StringStruct)) {
			final String printedObject = printer.print(javaClassName);
			throw new TypeErrorException("DECLARE: JAVA-CLASS-NAME must be a String. Got: " + printedObject);
		}
		final StringStruct javaClassNameString = (StringStruct) javaClassName;
		return new JavaClassNameDeclarationStruct(javaClassNameString.getAsJavaString());
	}

	private JavaClassNameDeclarationStruct saLispNameDeclaration(final List<LispStruct> declSpecBody) {

		final int declSpecBodySize = declSpecBody.size();
		if (declSpecBodySize != 1) {
			throw new ProgramErrorException("DECLARE: Incorrect number of arguments for LISP-NAME declaration: " + declSpecBodySize + ". Expected 1 argument.");
		}

		final LispStruct lispName = declSpecBody.get(0);
		if (!(lispName instanceof SymbolStruct)) {
			final String printedObject = printer.print(lispName);
			throw new TypeErrorException("DECLARE: LISP-NAME must be a Symbol. Got: " + printedObject);
		}
		final SymbolStruct lispNameSymbol = (SymbolStruct) lispName;

		final String name = lispNameSymbol.getName().replace('-', '_');
		final String realName = name.chars()
		                            .filter(Character::isJavaIdentifierPart)
		                            .mapToObj(e -> (char) e)
		                            .map(String::valueOf)
		                            .collect(Collectors.joining());
		final String lispNameClassName = realName + '_' + System.nanoTime();

		return new JavaClassNameDeclarationStruct(lispNameClassName);
	}

	private List<SpecialDeclarationStruct> saSpecialDeclaration(final List<LispStruct> declSpecBody) {

		final List<SpecialDeclarationStruct> specialDeclarationElements = new ArrayList<>(declSpecBody.size());

		// Special declaration can apply to multiple SymbolStructs
		for (final LispStruct declSpecBodyElement : declSpecBody) {
			if (!(declSpecBodyElement instanceof SymbolStruct)) {
				final String printedObject = printer.print(declSpecBodyElement);
				throw new ProgramErrorException("DECLARE: a non-symbol cannot be made SPECIAL: " + printedObject);
			}

			final SymbolStruct sym = (SymbolStruct) declSpecBodyElement;

			final SpecialDeclarationStruct specialDeclarationElement = new SpecialDeclarationStruct(sym);
			specialDeclarationElements.add(specialDeclarationElement);
		}

		return specialDeclarationElements;
	}
}
