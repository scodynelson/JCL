package jcl.system;

import java.util.Arrays;
import java.util.List;

import jcl.functions.PredicateFunctions;
import jcl.lang.FunctionStruct;

class BootstrapFunctions {

	static void bootstrap() throws Exception {
		bootstrapPredicateFunctions();
	}

	private static void bootstrapPredicateFunctions() throws Exception {
		final List<FunctionStruct> functions = Arrays.asList(
				new PredicateFunctions.ArrayPFunction(),
				new PredicateFunctions.AtomFunction(),
				new PredicateFunctions.BitVectorPFunction(),
				new PredicateFunctions.CharacterPFunction(),
				new PredicateFunctions.CompiledFunctionPFunction(),
				new PredicateFunctions.ComplexPFunction(),
				new PredicateFunctions.ConsPFunction(),
				new PredicateFunctions.FloatPFunction(),
				new PredicateFunctions.FunctionPFunction(),
				new PredicateFunctions.HashTablePFunction(),
				new PredicateFunctions.IntegerPFunction(),
				new PredicateFunctions.ListPFunction(),
				new PredicateFunctions.NullFunction(),
				new PredicateFunctions.NumberPFunction(),
				new PredicateFunctions.PackagePFunction(),
				new PredicateFunctions.PathnamePFunction(),
				new PredicateFunctions.RationalPFunction(),
				new PredicateFunctions.ReadtablePFunction(),
				new PredicateFunctions.RealPFunction(),
				new PredicateFunctions.RandomStatePFunction(),
				new PredicateFunctions.SimpleBitVectorPFunction(),
				new PredicateFunctions.SimpleStringPFunction(),
				new PredicateFunctions.SimpleVectorPFunction(),
				new PredicateFunctions.StringPFunction(),
				new PredicateFunctions.StreamPFunction(),
				new PredicateFunctions.SymbolPFunction(),
				new PredicateFunctions.VectorPFunction(),

				new PredicateFunctions.KeywordPFunction()
		);
		for (final FunctionStruct function : functions) {
			function.afterPropertiesSet();
		}
	}
}
