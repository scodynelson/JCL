/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
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
import jcl.compiler.real.struct.specialoperator.go.GoIntegerStructGenerator;
import jcl.compiler.real.struct.specialoperator.go.GoSymbolStruct;
import jcl.compiler.real.struct.specialoperator.go.GoSymbolStructGenerator;
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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class CodeGeneratorMediatorImpl implements CodeGeneratorMediator {

	@Autowired
	private ArrayCodeGenerator arrayCodeGenerator;

	@Autowired
	private BitVectorCodeGenerator bitVectorCodeGenerator;

	@Autowired
	private StringCodeGenerator stringCodeGenerator;

	@Autowired
	private VectorCodeGenerator vectorCodeGenerator;

	@Autowired
	private CharacterCodeGenerator characterCodeGenerator;

	@Autowired
	private ValuesCodeGenerator valuesCodeGenerator;

	@Autowired
	private NullCodeGenerator nullCodeGenerator;

	@Autowired
	private ConsCodeGenerator consCodeGenerator;

	@Autowired
	private ComplexCodeGenerator complexCodeGenerator;

	@Autowired
	private FloatCodeGenerator floatCodeGenerator;

	@Autowired
	private IntegerCodeGenerator integerCodeGenerator;

	@Autowired
	private RatioCodeGenerator ratioCodeGenerator;

	@Autowired
	private PathnameCodeGenerator pathnameCodeGenerator;

	@Autowired
	private KeywordCodeGenerator keywordCodeGenerator;

	@Autowired
	private NILCodeGenerator nilCodeGenerator;

	@Autowired
	private SymbolBindingCodeGenerator symbolBindingCodeGenerator;

	@Autowired
	private TCodeGenerator tCodeGenerator;

	@Autowired
	private BlockCodeGenerator blockCodeGenerator;

	@Autowired
	private CatchCodeGenerator catchCodeGenerator;

	@Autowired
	private FletCodeGenerator fletCodeGenerator;

	@Autowired
	private FunctionCallCodeGenerator functionCallCodeGenerator;

	@Autowired
	private IfCodeGenerator ifCodeGenerator;

	@Autowired
	private ImmutableLoadTimeValueCodeGenerator immutableLoadTimeValueCodeGenerator;

	@Autowired
	private LabelsCodeGenerator labelsCodeGenerator;

	@Autowired
	private LambdaFunctionCodeGenerator lambdaFunctionCodeGenerator;

	@Autowired
	private LambdaFunctionCallCodeGenerator lambdaFunctionCallCodeGenerator;

	@Autowired
	private LetCodeGenerator letCodeGenerator;

	@Autowired
	private LetStarCodeGenerator letStarCodeGenerator;

	@Autowired
	private LocallyCodeGenerator locallyCodeGenerator;

	@Autowired
	private MultipleValueCallCodeGenerator multipleValueCallCodeGenerator;

	@Autowired
	private MultipleValueProg1CodeGenerator multipleValueProg1CodeGenerator;

	@Autowired
	private MutableLoadTimeValueCodeGenerator mutableLoadTimeValueCodeGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Autowired
	private ProgvCodeGenerator progvCodeGenerator;

	@Autowired
	private QuoteCodeGenerator quoteCodeGenerator;

	@Autowired
	private ReturnFromCodeGenerator returnFromCodeGenerator;

	@Autowired
	private SetqCodeGenerator setqCodeGenerator;

	@Autowired
	private SymbolFunctionCodeGenerator symbolFunctionCodeGenerator;

	@Autowired
	private TagbodyCodeGenerator tagbodyCodeGenerator;

	@Autowired
	private TheCodeGenerator theCodeGenerator;

	@Autowired
	private ThrowCodeGenerator throwCodeGenerator;

	@Autowired
	private UnwindProtectCodeGenerator unwindProtectCodeGenerator;

	@Autowired
	private DefstructCodeGenerator defstructCodeGenerator;

	@Autowired
	private GoCodeGenerator goCodeGenerator;

	@Autowired
	private LambdaCodeGenerator lambdaCodeGenerator;

	@Override
	public void generate(final LispStruct input, final GeneratorState generatorState) {
		throw new RuntimeException("ICG: Found thing I can't generate code for class: " + input.getClass().getName());
	}

	@Override
	public void generate(final ArrayStruct<LispStruct> input, final GeneratorState generatorState) {
		arrayCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final BitVectorStruct input, final GeneratorState generatorState) {
		bitVectorCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final StringStruct input, final GeneratorState generatorState) {
		stringCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final VectorStruct<LispStruct> input, final GeneratorState generatorState) {
		vectorCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final CharacterStruct input, final GeneratorState generatorState) {
		characterCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final ValuesStruct input, final GeneratorState generatorState) {
		valuesCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final NullStruct input, final GeneratorState generatorState) {
		nullCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final ConsStruct input, final GeneratorState generatorState) {
		consCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final ComplexStruct input, final GeneratorState generatorState) {
		complexCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final FloatStruct input, final GeneratorState generatorState) {
		floatCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final IntegerStruct input, final GeneratorState generatorState) {
		integerCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final RatioStruct input, final GeneratorState generatorState) {
		ratioCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final PathnameStruct input, final GeneratorState generatorState) {
		pathnameCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final KeywordStruct input, final GeneratorState generatorState) {
		keywordCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final NILStruct input, final GeneratorState generatorState) {
		nilCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final SymbolStruct<LispStruct> input, final GeneratorState generatorState) {
		symbolBindingCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final TStruct input, final GeneratorState generatorState) {
		tCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final BlockStruct input, final GeneratorState generatorState) {
		blockCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final CatchStruct input, final GeneratorState generatorState) {
		catchCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final FletStruct input, final GeneratorState generatorState) {
		fletCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final FunctionCallStruct input, final GeneratorState generatorState) {
		functionCallCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final IfStruct input, final GeneratorState generatorState) {
		ifCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final ImmutableLoadTimeValueStruct input, final GeneratorState generatorState) {
		immutableLoadTimeValueCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final LabelsStruct input, final GeneratorState generatorState) {
		labelsCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final LambdaCompilerFunctionStruct input, final GeneratorState generatorState) {
		lambdaFunctionCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final LambdaFunctionCallStruct input, final GeneratorState generatorState) {
		lambdaFunctionCallCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final LetStruct input, final GeneratorState generatorState) {
		letCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final LetStarStruct input, final GeneratorState generatorState) {
		letStarCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final LocallyStruct input, final GeneratorState generatorState) {
		locallyCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final MultipleValueCallStruct input, final GeneratorState generatorState) {
		multipleValueCallCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final MultipleValueProg1Struct input, final GeneratorState generatorState) {
		multipleValueProg1CodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final MutableLoadTimeValueStruct input, final GeneratorState generatorState) {
		mutableLoadTimeValueCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final PrognStruct input, final GeneratorState generatorState) {
		prognCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final ProgvStruct input, final GeneratorState generatorState) {
		progvCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final QuoteStruct input, final GeneratorState generatorState) {
		quoteCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final ReturnFromStruct input, final GeneratorState generatorState) {
		returnFromCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final SetqStruct input, final GeneratorState generatorState) {
		setqCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final SymbolCompilerFunctionStruct input, final GeneratorState generatorState) {
		symbolFunctionCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final TagbodyStruct input, final GeneratorState generatorState) {
		tagbodyCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final TheStruct input, final GeneratorState generatorState) {
		theCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final ThrowStruct input, final GeneratorState generatorState) {
		throwCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final UnwindProtectStruct input, final GeneratorState generatorState) {
		unwindProtectCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final DefstructStruct input, final GeneratorState generatorState) {
		defstructCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final GoIntegerStruct input, final GeneratorState generatorState) {
		goCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final GoSymbolStruct input, final GeneratorState generatorState) {
		goCodeGenerator.generate(input, generatorState);
	}

	@Override
	public void generate(final LambdaStruct input, final GeneratorState generatorState) {
		lambdaCodeGenerator.generate(input, generatorState);
	}
}
