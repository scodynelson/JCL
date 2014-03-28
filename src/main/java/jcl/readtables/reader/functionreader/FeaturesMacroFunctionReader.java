package jcl.readtables.reader.functionreader;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariable;
import jcl.readtables.reader.Reader;
import jcl.symbols.SymbolStruct;
import jcl.variables.FeaturesVariable;
import jcl.readtables.reader.ReadSuppressVariable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

public class FeaturesMacroFunctionReader {

	private static final Logger LOGGER = LoggerFactory.getLogger(FeaturesMacroFunctionReader.class);

	private final Reader reader;

	public FeaturesMacroFunctionReader(final Reader reader) {
		this.reader = reader;
	}

	public void readFeatures(final boolean shouldHideFeatures) {

		boolean isFeature;

		final PackageStruct previousPackage = PackageVariable.INSTANCE.getValue();
		final boolean previousReadSuppress = ReadSuppressVariable.INSTANCE.getValue();
		try {
			PackageVariable.INSTANCE.setValue(GlobalPackageStruct.KEYWORD);
			ReadSuppressVariable.INSTANCE.setValue(false);

			final LispStruct token = reader.read();

			isFeature = isFeature(token);
		} catch (final ReaderErrorException ree) {
			LOGGER.debug(ree.getMessage(), ree);
			isFeature = false;
		} finally {
			PackageVariable.INSTANCE.setValue(previousPackage);
		}

		if (isFeature && shouldHideFeatures) {

			ReadSuppressVariable.INSTANCE.setValue(true);
			reader.read();
			ReadSuppressVariable.INSTANCE.setValue(previousReadSuppress);
		}
	}

	// TODO: We REALLY need to do this better at some point...
	private static boolean isFeature(final LispStruct token) {

		final boolean returnVal;

		if (token instanceof ConsStruct) {
			final ListStruct listStruct = (ListStruct) token;

			final LispStruct firstToken = listStruct.getFirst();
			final List<LispStruct> restTokens = listStruct.getRest().getAsJavaList();

			final SymbolStruct<?> symbolToken = (SymbolStruct<?>) firstToken;

			switch (symbolToken.getName().toUpperCase()) {
				case "NOT":
					returnVal = !isFeature(restTokens.get(0));
					break;
				case "AND":

					boolean tempReturnVal = true;
					for (final LispStruct lispToken : restTokens) {
						tempReturnVal = tempReturnVal && isFeature(lispToken);
					}
					returnVal = tempReturnVal;
					break;
				case "OR":

					boolean tempReturnVal2 = false;
					for (final LispStruct lispToken2 : restTokens) {
						tempReturnVal2 = tempReturnVal2 || isFeature(lispToken2);
					}
					returnVal = tempReturnVal2;
					break;
				default:
					throw new ReaderErrorException("Unknown operator in feature expression: " + symbolToken.getValue());
			}
		} else if (token instanceof SymbolStruct) {
			final SymbolStruct<?> symbolToken = (SymbolStruct<?>) token;

			final List<SymbolStruct<?>> featuresList = FeaturesVariable.INSTANCE.getValue();
			returnVal = featuresList.contains(symbolToken);
		} else {
			throw new ReaderErrorException("");
		}

		return returnVal;
	}
}
