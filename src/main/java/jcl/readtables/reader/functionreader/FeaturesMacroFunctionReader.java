package jcl.readtables.reader.functionreader;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariable;
import jcl.readtables.reader.ReadSuppressVariable;
import jcl.readtables.reader.Reader;
import jcl.symbols.KeywordSymbolStruct;
import jcl.variables.FeaturesVariable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

public class FeaturesMacroFunctionReader {

	private static final Logger LOGGER = LoggerFactory.getLogger(FeaturesMacroFunctionReader.class);

	private static final KeywordSymbolStruct NOT = new KeywordSymbolStruct("NOT");
	private static final KeywordSymbolStruct AND = new KeywordSymbolStruct("AND");
	private static final KeywordSymbolStruct OR = new KeywordSymbolStruct("OR");

	private final Reader reader;

	public FeaturesMacroFunctionReader(final Reader reader) {
		this.reader = reader;
	}

	public void readFeatures(final boolean shouldHideFeatures) {

		final boolean previousReadSuppress = ReadSuppressVariable.INSTANCE.getValue();
		final PackageStruct previousPackage = PackageVariable.INSTANCE.getValue();
		try {
			ReadSuppressVariable.INSTANCE.setValue(false);

			PackageVariable.INSTANCE.setValue(GlobalPackageStruct.KEYWORD);
			final LispStruct lispStruct = reader.read();
			PackageVariable.INSTANCE.setValue(previousPackage);

			final boolean isFeature = isFeature(lispStruct);
			if (isFeature && shouldHideFeatures) {
				ReadSuppressVariable.INSTANCE.setValue(true);
				reader.read();
			}
		} catch (final ReaderErrorException ree) {
			LOGGER.debug(ree.getMessage(), ree);
		} finally {
			PackageVariable.INSTANCE.setValue(previousPackage);
			ReadSuppressVariable.INSTANCE.setValue(previousReadSuppress);
		}
	}

	private static boolean isFeature(final LispStruct lispStruct) {
		if (lispStruct instanceof ListStruct) {
			return isListFeature((ListStruct) lispStruct);
		} else {
			final List<LispStruct> featuresList = FeaturesVariable.INSTANCE.getValue();
			return featuresList.contains(lispStruct);
		}
	}

	private static boolean isListFeature(final ListStruct listStruct) {
		return (listStruct instanceof ConsStruct) && isConsFeature((ConsStruct) listStruct);
	}

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
