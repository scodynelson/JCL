package jcl.compiler.real.sa.analyzer.declare;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.SymbolAnalyzer;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.compiler.real.struct.specialoperator.declare.SpecialDeclarationStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
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
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(DeclarationStruct.SPECIAL)) {
				final List<SpecialDeclarationStruct> sdes = saSpecialDeclaration(environment, declSpecBody.getAsJavaList());
				declareElement.getSpecialDeclarationElements().addAll(sdes);
			} else if (declIdentifier.equals(DeclarationStruct.TYPE)) {
				//we don't do anything here yet
			} else {
				throw new ProgramErrorException("DECLARE: Declaration specifier not supported: " + declIdentifier);
			}
		}

		return declareElement;
	}

	private List<SpecialDeclarationStruct> saSpecialDeclaration(final Environment environment, final List<LispStruct> declSpecBody) {

		final List<SpecialDeclarationStruct> specialDeclarationElements = new ArrayList<>(declSpecBody.size());

		// Special declaration can apply to multiple SymbolStructs
		for (final LispStruct declSpecBodyElement : declSpecBody) {
			if (!(declSpecBodyElement instanceof SymbolStruct)) {
				throw new ProgramErrorException("DECLARE: a non-SymbolStruct entity cannot be made SPECIAL: " + declSpecBodyElement);
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
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(symbolAnalyzer)
		                                                                .toString();
	}
}
