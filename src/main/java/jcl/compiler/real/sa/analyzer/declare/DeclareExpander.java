package jcl.compiler.real.sa.analyzer.declare;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.SymbolAnalyzer;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.JavaClassNameDeclarationStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.DeclarationStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DeclareExpander extends MacroFunctionExpander<DeclareStruct> {

	private static final long serialVersionUID = -27949883247210201L;

	@Autowired
	private SymbolAnalyzer symbolAnalyzer;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the declare macro function and adds it to the special operator 'declare'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.DECLARE.setMacroFunctionExpander(this);
	}

	@Override
	public DeclareStruct expand(final ListStruct form, final Environment environment) {

		final List<LispStruct> declSpecs = form.getRest().getAsJavaList();

		final DeclareStruct declareElement = new DeclareStruct();

		for (final LispStruct declSpec : declSpecs) {

			if (!(declSpec instanceof ListStruct)) {
				throw new ProgramErrorException("DECLARE: Declaration specifier must be of type ListStruct. Got: " + declSpec);
			}

			final ListStruct declSpecList = (ListStruct) declSpec;
			final LispStruct declIdentifier = declSpecList.getFirst();
			final ListStruct declSpecBody = declSpecList.getRest();

			final List<LispStruct> declSpecBodyAsJavaList = declSpecBody.getAsJavaList();

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
				final JavaClassNameDeclarationStruct jclds = saJavaClassNameDeclaration(declSpecBodyAsJavaList);
				declareElement.setJavaClassNameDeclaration(jclds);
			} else if (declIdentifier.equals(DeclarationStruct.SPECIAL)) {
				final List<SpecialDeclarationStruct> sds = saSpecialDeclaration(environment, declSpecBodyAsJavaList);
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
			throw new ProgramErrorException("DECLARE: a non-string cannot be used for a JAVA-CLASS-NAME: " + printedObject);
		}

		final StringStruct javaClassNameString = (StringStruct) javaClassName;
		return new JavaClassNameDeclarationStruct(javaClassNameString.getAsJavaString());
	}

	private List<SpecialDeclarationStruct> saSpecialDeclaration(final Environment environment, final List<LispStruct> declSpecBody) {

		final List<SpecialDeclarationStruct> specialDeclarationElements = new ArrayList<>(declSpecBody.size());

		// Special declaration can apply to multiple SymbolStructs
		for (final LispStruct declSpecBodyElement : declSpecBody) {
			if (!(declSpecBodyElement instanceof SymbolStruct)) {
				final String printedObject = printer.print(declSpecBodyElement);
				throw new ProgramErrorException("DECLARE: a non-symbol cannot be made SPECIAL: " + printedObject);
			}

			final SymbolStruct<?> sym = (SymbolStruct<?>) declSpecBodyElement;

			symbolAnalyzer.analyzeDynamic(sym, environment);

			final SpecialDeclarationStruct specialDeclarationElement = new SpecialDeclarationStruct(sym);
			specialDeclarationElements.add(specialDeclarationElement);
		}

		return specialDeclarationElements;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(symbolAnalyzer)
		                            .append(printer)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final DeclareExpander rhs = (DeclareExpander) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(symbolAnalyzer, rhs.symbolAnalyzer)
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(symbolAnalyzer)
		                                                                .append(printer)
		                                                                .toString();
	}
}
