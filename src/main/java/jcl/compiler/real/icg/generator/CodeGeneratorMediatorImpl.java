/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.arrays.ArrayStruct;
import jcl.arrays.BitVectorStruct;
import jcl.arrays.StringStruct;
import jcl.arrays.VectorStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.compiler.real.struct.specialoperator.BlockStruct;
import jcl.compiler.real.struct.specialoperator.CatchStruct;
import jcl.compiler.real.struct.specialoperator.FletStruct;
import jcl.compiler.real.struct.specialoperator.FunctionCallStruct;
import jcl.compiler.real.struct.specialoperator.IfStruct;
import jcl.compiler.real.struct.specialoperator.ImmutableLoadTimeValueStruct;
import jcl.compiler.real.struct.specialoperator.LabelsStruct;
import jcl.compiler.real.struct.specialoperator.LambdaCompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.LambdaFunctionCallStruct;
import jcl.compiler.real.struct.specialoperator.LetStarStruct;
import jcl.compiler.real.struct.specialoperator.LetStruct;
import jcl.compiler.real.struct.specialoperator.LocallyStruct;
import jcl.compiler.real.struct.specialoperator.MultipleValueCallStruct;
import jcl.compiler.real.struct.specialoperator.MultipleValueProg1Struct;
import jcl.compiler.real.struct.specialoperator.MutableLoadTimeValueStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.ProgvStruct;
import jcl.compiler.real.struct.specialoperator.QuoteStruct;
import jcl.compiler.real.struct.specialoperator.ReturnFromStruct;
import jcl.compiler.real.struct.specialoperator.SetqStruct;
import jcl.compiler.real.struct.specialoperator.SymbolCompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.TagbodyStruct;
import jcl.compiler.real.struct.specialoperator.TheStruct;
import jcl.compiler.real.struct.specialoperator.ThrowStruct;
import jcl.compiler.real.struct.specialoperator.UnwindProtectStruct;
import jcl.compiler.real.struct.specialoperator.defstruct.DefstructStruct;
import jcl.compiler.real.struct.specialoperator.go.GoIntegerStruct;
import jcl.compiler.real.struct.specialoperator.go.GoSymbolStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.lists.ConsStruct;
import jcl.lists.NullStruct;
import jcl.numbers.ComplexStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RatioStruct;
import jcl.pathnames.PathnameStruct;
import jcl.symbols.KeywordStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;

public class CodeGeneratorMediatorImpl implements CodeGeneratorMediator {



	@Override
	public void generate(final Object input, final GenerationConstants generationConstants) {
		throw new RuntimeException("ICG: Found thing I can't generate code for class: " + input.getClass().getName());
	}

	@Override
	public void generate(final ArrayStruct<?> input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final BitVectorStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final StringStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final VectorStruct<?> input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final CharacterStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final ValuesStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final NullStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final ConsStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final ComplexStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final FloatStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final IntegerStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final RatioStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final PathnameStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final KeywordStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final NILStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final SymbolStruct<?> input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final TStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final BlockStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final CatchStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final FletStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final FunctionCallStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final IfStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final ImmutableLoadTimeValueStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final LabelsStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final LambdaCompilerFunctionStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final LambdaFunctionCallStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final LetStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final LetStarStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final LocallyStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final MultipleValueCallStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final MultipleValueProg1Struct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final MutableLoadTimeValueStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final PrognStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final ProgvStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final QuoteStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final ReturnFromStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final SetqStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final SymbolCompilerFunctionStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final TagbodyStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final TheStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final ThrowStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final UnwindProtectStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final DefstructStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final GoIntegerStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final GoSymbolStruct input, final GeneratorState generatorState) {

	}

	@Override
	public void generate(final LambdaStruct input, final GeneratorState generatorState) {

	}
}
