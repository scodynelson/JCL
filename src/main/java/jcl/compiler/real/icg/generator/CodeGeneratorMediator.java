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

public interface CodeGeneratorMediator {

	void generate(Object input, final GenerationConstants generationConstants);
	
	void generate(ArrayStruct<?> input, final GeneratorState generatorState);
	void generate(BitVectorStruct input, final GeneratorState generatorState);
	void generate(StringStruct input, final GeneratorState generatorState);
	void generate(VectorStruct<?> input, final GeneratorState generatorState);
	
	void generate(CharacterStruct input, final GeneratorState generatorState);
	
	void generate(ValuesStruct input, final GeneratorState generatorState);

	void generate(NullStruct input, final GeneratorState generatorState);
	void generate(ConsStruct input, final GeneratorState generatorState);
	
	void generate(ComplexStruct input, final GeneratorState generatorState);
	void generate(FloatStruct input, final GeneratorState generatorState);
	void generate(IntegerStruct input, final GeneratorState generatorState);
	void generate(RatioStruct input, final GeneratorState generatorState);
	
	void generate(PathnameStruct input, final GeneratorState generatorState);
	
	void generate(KeywordStruct input, final GeneratorState generatorState);
	void generate(NILStruct input, final GeneratorState generatorState);
	void generate(SymbolStruct<?> input, final GeneratorState generatorState);
	void generate(TStruct input, final GeneratorState generatorState);
	
	void generate(BlockStruct input, final GeneratorState generatorState);
	void generate(CatchStruct input, final GeneratorState generatorState);
	void generate(FletStruct input, final GeneratorState generatorState);
	void generate(FunctionCallStruct input, final GeneratorState generatorState);
	void generate(IfStruct input, final GeneratorState generatorState);
	void generate(ImmutableLoadTimeValueStruct input, final GeneratorState generatorState);
	void generate(LabelsStruct input, final GeneratorState generatorState);
	void generate(LambdaCompilerFunctionStruct input, final GeneratorState generatorState);
	void generate(LambdaFunctionCallStruct input, final GeneratorState generatorState);
	void generate(LetStruct input, final GeneratorState generatorState);
	void generate(LetStarStruct input, final GeneratorState generatorState);
	void generate(LocallyStruct input, final GeneratorState generatorState);
	void generate(MultipleValueCallStruct input, final GeneratorState generatorState);
	void generate(MultipleValueProg1Struct input, final GeneratorState generatorState);
	void generate(MutableLoadTimeValueStruct input, final GeneratorState generatorState);
	void generate(PrognStruct input, final GeneratorState generatorState);
	void generate(ProgvStruct input, final GeneratorState generatorState);
	void generate(QuoteStruct input, final GeneratorState generatorState);
	void generate(ReturnFromStruct input, final GeneratorState generatorState);
	void generate(SetqStruct input, final GeneratorState generatorState);
	void generate(SymbolCompilerFunctionStruct input, final GeneratorState generatorState);
	void generate(TagbodyStruct input, final GeneratorState generatorState);
	void generate(TheStruct input, final GeneratorState generatorState);
	void generate(ThrowStruct input, final GeneratorState generatorState);
	void generate(UnwindProtectStruct input, final GeneratorState generatorState);

	void generate(DefstructStruct input, final GeneratorState generatorState);
	
	void generate(GoIntegerStruct input, final GeneratorState generatorState);
	void generate(GoSymbolStruct input, final GeneratorState generatorState);
	
	void generate(LambdaStruct input, final GeneratorState generatorState);
}
