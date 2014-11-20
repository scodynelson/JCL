package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.packages.PackageStruct;
import jcl.structs.symbols.BooleanStruct;
import jcl.structs.symbols.KeywordSymbolStruct;
import jcl.structs.symbols.NILStruct;
import jcl.structs.symbols.TStruct;
import jcl.structs.symbols.variables.Variable;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * Reader Macro Function for handling the reading of *features* in the system, handling whether or not those specific
 * features should be hidden or not (aka. the token is read in but ignored).
 */
abstract class FeaturesReaderMacroFunction extends ReaderMacroFunction {

	private static final Logger LOGGER = LoggerFactory.getLogger(FeaturesReaderMacroFunction.class);

	private static final KeywordSymbolStruct NOT = new KeywordSymbolStruct("NOT");
	private static final KeywordSymbolStruct AND = new KeywordSymbolStruct("AND");
	private static final KeywordSymbolStruct OR = new KeywordSymbolStruct("OR");

	private final boolean shouldHideFeatures;

	/**
	 * Protected constructor.
	 *
	 * @param shouldHideFeatures
	 * 		whether or not the *features* read should be hidden or not (aka. the token is read in but ignored)
	 */
	protected FeaturesReaderMacroFunction(final boolean shouldHideFeatures) {
		this.shouldHideFeatures = shouldHideFeatures;
	}

	/**
	 * Reads in the next set of *features*, following the {@link #shouldHideFeatures} property to properly suppress the
	 * read operation or not.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read in the next token
	 */
	protected void readFeatures(final Reader reader) {
		final BooleanStruct<?> previousReadSuppress = Variable.READ_SUPPRESS.getValue();
		final PackageStruct previousPackage = Variable.PACKAGE.getValue();
		try {
			Variable.READ_SUPPRESS.setValue(NILStruct.INSTANCE);

			Variable.PACKAGE.setValue(GlobalPackageStruct.KEYWORD);
			final LispStruct lispStruct = reader.read();
			Variable.PACKAGE.setValue(previousPackage);

			final boolean isFeature = isFeature(lispStruct);
			if (isFeature && shouldHideFeatures) {
				Variable.READ_SUPPRESS.setValue(TStruct.INSTANCE);
				reader.read();
			}
		} catch (final ReaderErrorException ree) {
			LOGGER.debug(ree.getMessage(), ree);
		} finally {
			Variable.PACKAGE.setValue(previousPackage);
			Variable.READ_SUPPRESS.setValue(previousReadSuppress);
		}
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}

	/**
	 * Determines if the provided {@link LispStruct} is a feature that should be read in or not.
	 *
	 * @param lispStruct
	 * 		the {@link LispStruct} used to determine if a feature should be read in or not
	 *
	 * @return true if the provided {@link LispStruct} is a feature that should be read in; false otherwise
	 */
	private static boolean isFeature(final LispStruct lispStruct) {
		if (lispStruct instanceof ListStruct) {
			return isListFeature((ListStruct) lispStruct);
		} else {
			final List<LispStruct> featuresList = Variable.FEATURES.getValue().getAsJavaList();
			return featuresList.contains(lispStruct);
		}
	}

	/**
	 * Determines if the provided {@link ListStruct} is a feature that should be read in or not. If it is not an
	 * instance of {@link ConsStruct}, it is automatically not a feature.
	 *
	 * @param listStruct
	 * 		the {@link ListStruct} used to determine if a feature should be read in or not
	 *
	 * @return true if the provided {@link ListStruct} is a feature that should be read in; false otherwise
	 */
	private static boolean isListFeature(final ListStruct listStruct) {
		return (listStruct instanceof ConsStruct) && isConsFeature((ConsStruct) listStruct);
	}

	/**
	 * Determines if the provided {@link ConsStruct} is a feature that should be read in or not.
	 *
	 * @param consStruct
	 * 		the {@link ConsStruct} used to determine if a feature should be read in or not
	 *
	 * @return true if the provided {@link ConsStruct} is a feature that should be read in; false otherwise
	 */
	private static boolean isConsFeature(final ConsStruct consStruct) {
		final LispStruct first = consStruct.getFirst();
		final List<LispStruct> rest = consStruct.getRest().getAsJavaList();

		if (!(first instanceof KeywordSymbolStruct)) {
			throw new ReaderErrorException("First element of feature expression must be either: :NOT, :AND, or :OR.");
		}

		final KeywordSymbolStruct featureOperator = (KeywordSymbolStruct) first;
		if (featureOperator.equals(NOT)) {
			return !isFeature(rest.get(0));
		} else if (featureOperator.equals(AND)) {
			boolean tempReturnVal = true;
			for (final LispStruct lispStruct : rest) {
				tempReturnVal = tempReturnVal && isFeature(lispStruct);
			}
			return tempReturnVal;
		} else if (featureOperator.equals(OR)) {
			boolean tempReturnVal2 = false;
			for (final LispStruct lispStruct : rest) {
				tempReturnVal2 = tempReturnVal2 || isFeature(lispStruct);
			}
			return tempReturnVal2;
		} else {
			throw new ReaderErrorException("Unknown operator in feature expression: " + featureOperator);
		}
	}
}
