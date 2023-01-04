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
					"jcl/base-macro-lambdas.lisp",
					"jcl/sequences.lisp",
					"jcl/macros.lisp",
					"jcl/iterators.lisp",
					"jcl/conditions.lisp",
					"jcl/characters.lisp",
					"jcl/pathnames.lisp",
					"jcl/symbols.lisp",
					"jcl/strings.lisp",
					"jcl/streams.lisp",
					"jcl/reader.lisp",
					"jcl/packages.lisp",
					"jcl/lists.lisp",
					"jcl/numbers.lisp",
					"jcl/hashtables.lisp",
					"jcl/setf.lisp",
					"jcl/files.lisp",
					"jcl/environment.lisp",
					"jcl/structures.lisp"
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
