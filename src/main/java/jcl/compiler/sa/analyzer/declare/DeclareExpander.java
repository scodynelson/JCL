package jcl.compiler.sa.analyzer.declare;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.struct.specialoperator.declare.JavaClassNameDeclarationStruct;
import jcl.compiler.struct.specialoperator.declare.LispNameDeclarationStruct;
import jcl.compiler.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.lang.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class DeclareExpander extends MacroFunctionExpander<DeclareStruct> {

	public static final DeclareExpander INSTANCE = new DeclareExpander();

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.DECLARE;
	}

	@Override
	public DeclareStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // DECLARE SYMBOL

		final List<LispStruct> declSpecs = new ArrayList<>();
		iterator.forEachRemaining(declSpecs::add);

		final DeclareStruct declareElement = new DeclareStruct();

		for (final LispStruct declSpec : declSpecs) {
			if (!(declSpec instanceof final ListStruct declSpecList)) {
				throw new TypeErrorException("DECLARE: DECLARATION-SPECIFIER must be a List. Got: " + declSpec);
			}

			final Iterator<LispStruct> declSpecIterator = declSpecList.iterator();
			final LispStruct declIdentifier = declSpecIterator.next();


			final List<LispStruct> declSpecBody = new ArrayList<>();
			declSpecIterator.forEachRemaining(declSpecBody::add);

			// now come the various cases
			if (declIdentifier.eq(CommonLispSymbols.DECLARATION)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.eq(CommonLispSymbols.DYNAMIC_EXTENT)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.eq(CommonLispSymbols.FTYPE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.eq(CommonLispSymbols.IGNORABLE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.eq(CommonLispSymbols.IGNORE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.eq(CommonLispSymbols.INLINE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.eq(CommonLispSymbols.NOTINLINE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.eq(CommonLispSymbols.OPTIMIZE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.eq(CommonLispSymbols.JAVA_CLASS_NAME)) {
				final JavaClassNameDeclarationStruct jclds = saJavaClassNameDeclaration(declSpecBody);
				declareElement.setJavaClassNameDeclaration(jclds);
			} else if (declIdentifier.eq(CommonLispSymbols.LISP_NAME)) {
				final LispNameDeclarationStruct lnds = saLispNameDeclaration(declSpecBody);
				declareElement.setLispNameDeclarationStruct(lnds);
			} else if (declIdentifier.eq(CommonLispSymbols.SPECIAL)) {
				final List<SpecialDeclarationStruct> sds = saSpecialDeclaration(declSpecBody);
				declareElement.getSpecialDeclarations().addAll(sds);
			} else if (declIdentifier.eq(CommonLispSymbols.TYPE)) {
				//TODO: we don't do anything here yet
			} else {
				throw new ProgramErrorException("DECLARE: Declaration specifier not supported: " + declIdentifier);
			}
		}

		return declareElement;
	}

	private static JavaClassNameDeclarationStruct saJavaClassNameDeclaration(final List<LispStruct> declSpecBody) {

		final int declSpecBodySize = declSpecBody.size();
		if (declSpecBodySize != 1) {
			throw new ProgramErrorException("DECLARE: Incorrect number of arguments for JAVA-CLASS-NAME declaration: " + declSpecBodySize + ". Expected 1 argument.");
		}

		final LispStruct javaClassName = declSpecBody.get(0);
		if (!(javaClassName instanceof final StringStruct javaClassNameString)) {
			throw new TypeErrorException("DECLARE: JAVA-CLASS-NAME must be a String. Got: " + javaClassName);
		}
		return new JavaClassNameDeclarationStruct(javaClassNameString.toJavaString());
	}

	private static LispNameDeclarationStruct saLispNameDeclaration(final List<LispStruct> declSpecBody) {

		final int declSpecBodySize = declSpecBody.size();
		if (declSpecBodySize != 1) {
			throw new ProgramErrorException("DECLARE: Incorrect number of arguments for LISP-NAME declaration: " + declSpecBodySize + ". Expected 1 argument.");
		}

		final LispStruct lispName = declSpecBody.get(0);

		SymbolStruct functionSymbolName = null;

		final String name;
		if (lispName instanceof final SymbolStruct lispNameSymbol) {
			name = lispNameSymbol.getName().replace('-', '_');
			functionSymbolName = lispNameSymbol;
		} else if (lispName instanceof final ConsStruct lispNameCons) {

			final StringBuilder builder = new StringBuilder();
			for (final LispStruct current : lispNameCons) {
				if (current instanceof SymbolStruct) {
					builder.append(((SymbolStruct) current).getName())
					       .append('_');
				} else {
					throw new TypeErrorException("DECLARE: LISP-NAME that is a Cons must only contain Symbols. Got: " + lispName);
				}
			}

			builder.setLength(builder.length() - 1);
			name = builder.toString();
		} else {
			throw new TypeErrorException("DECLARE: LISP-NAME must be a Symbol or a Cons. Got: " + lispName);
		}

		final String realName = name.chars()
		                            .filter(Character::isJavaIdentifierPart)
		                            .mapToObj(e -> (char) e)
		                            .map(String::valueOf)
		                            .collect(Collectors.joining());
		final String lispNameClassName = realName + '_' + System.nanoTime();

		return new LispNameDeclarationStruct(functionSymbolName, lispNameClassName);
	}

	private static List<SpecialDeclarationStruct> saSpecialDeclaration(final List<LispStruct> declSpecBody) {

		final List<SpecialDeclarationStruct> specialDeclarationElements = new ArrayList<>(declSpecBody.size());

		// Special declaration can apply to multiple SymbolStructs
		for (final LispStruct declSpecBodyElement : declSpecBody) {
			if (!(declSpecBodyElement instanceof final SymbolStruct sym)) {
				throw new ProgramErrorException("DECLARE: a non-symbol cannot be made SPECIAL: " + declSpecBodyElement);
			}

			final SpecialDeclarationStruct specialDeclarationElement = new SpecialDeclarationStruct(sym);
			specialDeclarationElements.add(specialDeclarationElement);
		}

		return specialDeclarationElements;
	}
}
