/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator.special;

import java.util.Collections;
import java.util.List;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.functions.FunctionStruct;

public class TestLambdaGenerator extends FunctionStruct {

	private static final long serialVersionUID = 5616713628691655052L;

	public TestLambdaGenerator() {
		super("DocumentationString");
		initLambdaListBindings();
	}

	private void initLambdaListBindings() {
		final List<RequiredBinding> requiredBindings = Collections.emptyList();
		final List<OptionalBinding> optionalBindings = Collections.emptyList();
		final RestBinding restBinding = null;
		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		lambdaListBindings = new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return new CharacterStruct(97);
	}

	/*

<init>()V
00000 TestLambdaGenerator  :  :     ALOAD 0
00001 TestLambdaGenerator  : TestLambdaGenerator  :     LDC "DocumentationString"
00002 TestLambdaGenerator  : TestLambdaGenerator String  :     INVOKESPECIAL jcl/functions/FunctionStruct.<init> (Ljava/lang/String;)V
00003 TestLambdaGenerator  :  :     ALOAD 0
00004 TestLambdaGenerator  : TestLambdaGenerator  :     INVOKESPECIAL jcl/TestLambdaGenerator.initLambdaListBindings ()V
00005 TestLambdaGenerator  :  :     RETURN

initLambdaListBindings()V
00000 TestLambdaGenerator . . . . . .  :  :     INVOKESTATIC java/util/Collections.emptyList ()Ljava/util/List;
00001 TestLambdaGenerator . . . . . .  : List  :     ASTORE 1
00002 TestLambdaGenerator List . . . . .  :  :     INVOKESTATIC java/util/Collections.emptyList ()Ljava/util/List;
00003 TestLambdaGenerator List . . . . .  : List  :     ASTORE 2
00004 TestLambdaGenerator List List . . . .  :  :     ACONST_NULL
00005 TestLambdaGenerator List List . . . .  : Lnull;  :     ASTORE 3
00006 TestLambdaGenerator List List Lnull; . . .  :  :     INVOKESTATIC java/util/Collections.emptyList ()Ljava/util/List;
00007 TestLambdaGenerator List List Lnull; . . .  : List  :     ASTORE 4
00008 TestLambdaGenerator List List Lnull; List . .  :  :     ICONST_0
00009 TestLambdaGenerator List List Lnull; List . .  : I  :     ISTORE 5
00010 TestLambdaGenerator List List Lnull; List I .  :  :     INVOKESTATIC java/util/Collections.emptyList ()Ljava/util/List;
00011 TestLambdaGenerator List List Lnull; List I .  : List  :     ASTORE 6
00012 TestLambdaGenerator List List Lnull; List I List  :  :     ALOAD 0
00013 TestLambdaGenerator List List Lnull; List I List  : TestLambdaGenerator  :     NEW jcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings
00014 TestLambdaGenerator List List Lnull; List I List  : TestLambdaGenerator OrdinaryLambdaListBindings  :     DUP
00015 TestLambdaGenerator List List Lnull; List I List  : TestLambdaGenerator OrdinaryLambdaListBindings OrdinaryLambdaListBindings  :     ALOAD 1
00016 TestLambdaGenerator List List Lnull; List I List  : TestLambdaGenerator OrdinaryLambdaListBindings OrdinaryLambdaListBindings List  :     ALOAD 2
00017 TestLambdaGenerator List List Lnull; List I List  : TestLambdaGenerator OrdinaryLambdaListBindings OrdinaryLambdaListBindings List List  :     ALOAD 3
00018 TestLambdaGenerator List List Lnull; List I List  : TestLambdaGenerator OrdinaryLambdaListBindings OrdinaryLambdaListBindings List List Lnull;  :     ALOAD 4
00019 TestLambdaGenerator List List Lnull; List I List  : TestLambdaGenerator OrdinaryLambdaListBindings OrdinaryLambdaListBindings List List Lnull; List  :     ALOAD 6
00020 TestLambdaGenerator List List Lnull; List I List  : TestLambdaGenerator OrdinaryLambdaListBindings OrdinaryLambdaListBindings List List Lnull; List List  :     ICONST_0
00021 TestLambdaGenerator List List Lnull; List I List  : TestLambdaGenerator OrdinaryLambdaListBindings OrdinaryLambdaListBindings List List Lnull; List List I  :     INVOKESPECIAL jcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings.<init> (Ljava/util/List;Ljava/util/List;Ljcl/compiler/real/environment/binding/lambdalist/RestBinding;Ljava/util/List;Ljava/util/List;Z)V
00022 TestLambdaGenerator List List Lnull; List I List  : TestLambdaGenerator OrdinaryLambdaListBindings  :     PUTFIELD jcl/TestLambdaGenerator.lambdaListBindings : Ljcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings;
00023 TestLambdaGenerator List List Lnull; List I List  :  :     RETURN

apply([Ljcl/LispStruct;)Ljcl/LispStruct;
00000 TestLambdaGenerator LispStruct  :  :     NEW jcl/characters/CharacterStruct
00001 TestLambdaGenerator LispStruct  : CharacterStruct  :     DUP
00002 TestLambdaGenerator LispStruct  : CharacterStruct CharacterStruct  :     BIPUSH 97
00003 TestLambdaGenerator LispStruct  : CharacterStruct CharacterStruct I  :     INVOKESPECIAL jcl/characters/CharacterStruct.<init> (I)V
00004 TestLambdaGenerator LispStruct  : CharacterStruct  :     ARETURN

	 */
}
