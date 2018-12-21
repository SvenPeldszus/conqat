package org.conqat.engine.sourcecode.coverage.volume;

import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * {@ConQAT.Doc}
 *
 * @author $Author: kinnen $
 * @version $Rev: 51034 $
 * @ConQAT.Rating GREEN Hash: 9A35496D6A7C394A21255F1418AD5F4E
 */
@AConQATProcessor(description = "Reports about coverable lines.")
public class CoverableLineProcessor extends CoverableEntityProcessorBase {

	/**
	 * The token types that do not count towards lines. This is used for example
	 * to exclude lines containing only braces.
	 */
	private static final Set<ETokenType> EXCLUDED_TOKEN_TYPES = EnumSet.of(
			ETokenType.LBRACE, ETokenType.RBRACE);

	/** Constructor. */
	public CoverableLineProcessor() {
		super(EShallowEntityType.STATEMENT);
	}

	/** {@inheritDoc} */
	@Override
	protected Collection<ShallowEntity> filterEntities(
			List<ShallowEntity> entities) {
		return CoverableStatementProcessor
				.filterNonCoverableStatements(entities);
	}

	/** {@inheritDoc} */
	@Override
	protected List<LineHint> entitiesToString(Collection<ShallowEntity> entities) {
		Set<Integer> lines = determineLinesFromEntities(entities);
		List<LineHint> hints = new ArrayList<>();
		for (int line : CollectionUtils.sort(lines)) {
			hints.add(new LineHint("Line " + line, line));
		}
		return hints;
	}

	/** Determines the lines that are coverable from the given entities. */
	public static Set<Integer> determineLinesFromEntities(
			Collection<ShallowEntity> entities) {
		Set<Integer> lines = new HashSet<>();
		for (ShallowEntity entity : entities) {
			if (entity.getChildren().isEmpty()) {
				addLines(entity.includedTokens(), lines);
			} else {
				List<IToken> startTokens = entity.ownStartTokens();
				List<IToken> endTokens = entity.ownEndTokens();
				if (startTokens.size() >= endTokens.size()) {
					addLines(startTokens, lines);
				} else {
					addLines(endTokens, lines);
				}
			}
		}
		return lines;
	}

	/** Adds lines covered by the given tokens to the lines set. */
	private static void addLines(List<IToken> tokens, Set<Integer> lines) {
		for (IToken token : tokens) {
			if (EXCLUDED_TOKEN_TYPES.contains(token.getType())) {
				continue;
			}

			int lineCount = StringUtils.countLines(token.getText());
			for (int i = 0; i < lineCount; ++i) {
				// need +1 here, as tokens have 0-based line numbers
				lines.add(token.getLineNumber() + 1 + i);
			}
		}
	}
}
