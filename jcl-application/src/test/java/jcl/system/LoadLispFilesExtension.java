package jcl.system;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import jcl.compiler.function.CompileFileResult;
import jcl.compiler.function.InternalCompile;
import jcl.compiler.function.InternalLoad;
import jcl.compiler.sa.BootstrapExpanders;
import jcl.lang.LispStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.StringStruct;
import jcl.lang.TStruct;
import jcl.lang.statics.CommonLispSymbols;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ExtensionContext;

public class LoadLispFilesExtension implements BeforeAllCallback, ExtensionContext.Store.CloseableResource {

	private static boolean started;

	private Path tmpDir;

	@Override
	public void beforeAll(final ExtensionContext context) throws IOException {
		if (!started) {
			BootstrapSymbols.bootstrap();
			BootstrapFunctions.bootstrap();
			BootstrapExpanders.bootstrap();

			final List<String> sourceFiles = List.of(
					"jcl/compiler/base-macro-lambdas.lisp",
					"jcl/sequences/sequences.lisp",
					"jcl/lists/base-lists.lisp",
					"jcl/compiler/macros.lisp",
					"jcl/iterators/iterators.lisp",
					"jcl/characters/characters.lisp",
					"jcl/pathnames/pathnames.lisp",
					"jcl/symbols/symbols.lisp",
					"jcl/reader/reader.lisp",
					"jcl/strings/strings.lisp",
					"jcl/streams/streams.lisp",
					"jcl/packages/packages.lisp",
					"jcl/lists/lists.lisp",
					"jcl/numbers/numbers.lisp",
					"jcl/hashtables/hashtables.lisp",
					"jcl/environment/environment.lisp",
					"jcl/structures/structures.lisp"
			);

			tmpDir = Files.createTempDirectory(Paths.get(System.getProperty("user.dir")), "tests_");

			for (final String fileName : sourceFiles) {
				final PathnameStruct sourceFile = PathnameStruct.toPathname("src/main/lisp/" + fileName);

				final LispStruct pathnameName = sourceFile.pathnameName();
				final LispStruct pathnameType = StringStruct.toLispString("jar");

				// TODO: Can we fix Pathnames needing to have a trailing "/" for determining directory or file???
				final PathnameStruct destDirectory = PathnameStruct.toPathname(tmpDir.toFile().getAbsolutePath() + "/");
				final PathnameStruct newSourceFile = PathnameStruct.toPathname(
						destDirectory.pathnameHost(),
						destDirectory.pathnameDevice(),
						destDirectory.pathnameDirectory(),
						pathnameName,
						pathnameType,
						destDirectory.pathnameVersion()
				);

				final CompileFileResult compileFileResult = InternalCompile.compileFile(
						sourceFile,
						newSourceFile,
						TStruct.INSTANCE,
						TStruct.INSTANCE,
						CommonLispSymbols.DEFAULT_KEYWORD
				);
				InternalLoad.load(
						compileFileResult.getOutputTruename(),
						TStruct.INSTANCE,
						TStruct.INSTANCE,
						TStruct.INSTANCE,
						CommonLispSymbols.DEFAULT_KEYWORD
				);
			}

			started = true;
			// Your "before all tests" startup logic goes here
			// The following line registers a callback hook when the root test context is shut down
			context.getRoot().getStore(ExtensionContext.Namespace.GLOBAL).put("load lisp files", this);
		}
	}

	@Override
	public void close() throws IOException {
		if (tmpDir != null) {
			try (final DirectoryStream<Path> ds = Files.newDirectoryStream(tmpDir)) {
				for (final Path file : ds) {
					Files.delete(file);
				}
				Files.delete(tmpDir);
			}
		}
	}
}
