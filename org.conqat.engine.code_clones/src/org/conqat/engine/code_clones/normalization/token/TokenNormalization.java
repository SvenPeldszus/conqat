/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2011 The ConQAT Project                                   |
|                                                                          |
| Licensed under the Apache License, Version 2.0 (the "License");          |
| you may not use this file except in compliance with the License.         |
| You may obtain a copy of the License at                                  |
|                                                                          |
|    http://www.apache.org/licenses/LICENSE-2.0                            |
|                                                                          |
| Unless required by applicable law or agreed to in writing, software      |
| distributed under the License is distributed on an "AS IS" BASIS,        |
| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. |
| See the License for the specific language governing permissions and      |
| limitations under the License.                                           |
+-------------------------------------------------------------------------*/
package org.conqat.engine.code_clones.normalization.token;

import java.io.Serializable;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.conqat.engine.code_clones.core.CloneDetectionException;
import org.conqat.engine.code_clones.core.TokenUnit;
import org.conqat.engine.code_clones.detection.SentinelUnit;
import org.conqat.engine.code_clones.normalization.UnitProviderBase;
import org.conqat.engine.code_clones.normalization.token.configuration.ITokenConfiguration;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.regions.RegionSetDictionary;
import org.conqat.engine.resource.util.ResourceTraversalUtils;
import org.conqat.engine.resource.util.ResourceUtils;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.engine.sourcecode.resource.TokenElementUtils;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.IdManager;
import org.conqat.lib.commons.collections.TwoDimHashMap;
import org.conqat.lib.commons.filesystem.CanonicalFile;
import org.conqat.lib.commons.region.RegionSet;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * According to the token configuration (provided in constructor), this class
 * applies normalizing transformations and filtering to the token stream.
 * 
 * Additionally, this component performs the conversion from scanner tokens
 * {@link org.conqat.lib.scanner.IToken} to tokens units
 * {@link org.conqat.engine.code_clones.core.TokenUnit}.
 * 
 * This class is potentially serialized in the context of index-based clone
 * detection.
 * 
 * @author $Author: goeb $
 * @version $Revision: 51582 $
 * @ConQAT.Rating GREEN Hash: 1D40C9DA2F910FD0DD136FDD86450EB3
 */
public class TokenNormalization extends
		UnitProviderBase<ITokenResource, TokenUnit> implements Serializable {

	/** Version used for serialization. */
	private static final long serialVersionUID = 1;

	/** Set of all type keywords that can be normalized. */
	private static final Set<ETokenType> TYPE_KEYWORDS = EnumSet.copyOf(Arrays
			.asList(ETokenType.STRING, ETokenType.BOOL, ETokenType.BOOLEAN,
					ETokenType.DECIMAL, ETokenType.DOUBLE, ETokenType.FLOAT,
					ETokenType.SINGLE, ETokenType.CHAR, ETokenType.BYTE,
					ETokenType.OBJECT, ETokenType.INT));

	/** Number of upcoming tokens that are to be ignored */
	private int ignoreNextTokens = 0;

	/** The provider that yields the tokens that get normalized. */
	private final ITokenProvider tokenProvider;

	/**
	 * The {@link ITokenConfiguration} that determines how normalization is
	 * performed per default
	 */
	private final ITokenConfiguration defaultConfig;

	/**
	 * List of configurations used for normalization. Order of configuration
	 * names determines preference, in case more than one configuration matches
	 * a token.
	 */
	private final LinkedHashMap<String, ITokenConfiguration> configurations = new LinkedHashMap<String, ITokenConfiguration>();

	/**
	 * Maps from a uniform path and a configuration name to the
	 * {@link RegionSet} that determines for which parts of the element the
	 * configuration holds.
	 */
	private transient TwoDimHashMap<String, String, RegionSet> configurationMap;

	/** Index of unit in its file */
	private int indexInFile = 0;

	/** Maps from tokens to their normalized units. Used to write debug files */
	private transient Map<IToken, TokenUnit> normalizationMapping;

	/** Flag that can be set to turn off filtering of end of statement tokens */
	private boolean alwaysKeepEndOfStatementTokens = false;

	/**
	 * The root of the token resources whose content is normalized. This is used
	 * during initialization to extract the regions that select the
	 * normalization configuration to be used. Additionally, this is required
	 * when writing debug files.
	 */
	private transient ITokenResource tokenResourceRoot;

	/**
	 * Mapping from uniform path to location. This is only used for specific
	 * cases (e.g. writing debug files) and thus initialized in a lazy fashion.
	 * This should only be accessed via {@link #getElement(String)}.
	 */
	private transient Map<String, ITokenElement> uniformPathToElement;

	/**
	 * This manager is used for normalizing identifiers. Within a statement same
	 * identifiers are normalized with the same id. Example:
	 * 
	 * <pre>
	 * b = a * a;
	 * </pre>
	 * 
	 * becomes
	 * 
	 * <pre>
	 * id0 = id1 * id1;
	 * </pre>
	 */
	private transient IdManager<String> identifierManager;

	/**
	 * If this string is set to a non-empty value, a debug file (containing the
	 * normalized units) is written for each input file.
	 */
	private final String debugFileExtension;

	/**
	 * The previously returned token used for context-aware normalization. This
	 * is reset to null at sentinels and statement boundaries.
	 */
	private IToken previousToken = null;

	/**
	 * Create new {@link TokenNormalization} that does not write debug
	 * information
	 */
	public TokenNormalization(ITokenProvider tokenProvider,
			List<ITokenConfiguration> configurationList,
			ITokenConfiguration defaultConfig) {
		this(tokenProvider, configurationList, defaultConfig, null);
	}

	/**
	 * Create new {@link TokenNormalization} that optionally writes debug
	 * information
	 */
	public TokenNormalization(ITokenProvider tokenProvider,
			List<ITokenConfiguration> configurationList,
			ITokenConfiguration defaultConfig, String debugFileExtension) {
		this.tokenProvider = tokenProvider;
		this.defaultConfig = defaultConfig;
		this.debugFileExtension = debugFileExtension;

		for (ITokenConfiguration configuration : configurationList) {
			configurations.put(configuration.getName(), configuration);
		}
	}

	/** {@inheritDoc} */
	@Override
	protected void init(ITokenResource root) throws CloneDetectionException {
		tokenProvider.init(root, getLogger());
		tokenResourceRoot = root;
		uniformPathToElement = null;
		normalizationMapping = new LinkedHashMap<IToken, TokenUnit>();

		initRegions(root);
	}

	/** Perform initialization */
	private void initRegions(ITokenResource root) {
		configurationMap = new TwoDimHashMap<String, String, RegionSet>();
		identifierManager = new IdManager<String>();

		for (ITokenElement element : TokenElementUtils.listTokenElements(root)) {
			retrieveAndStoreRegions(element);
		}
	}

	/** Retrieve regions information and store in map */
	private void retrieveAndStoreRegions(ITokenElement element) {
		try {
			RegionSetDictionary dictionary = RegionSetDictionary
					.retrieve(element);
			if (dictionary == null) {
				return;
			}
			for (RegionSet regions : dictionary) {
				configurationMap.putValue(element.getUniformPath(),
						regions.getName(), regions);
			}
		} catch (ConQATException e) {
			getLogger().warn(
					"Could not access regions for element '"
							+ element.getName() + "': " + e.getMessage());
		}
	}

	/**
	 * This method consumes a token from the token structure, transforms it and
	 * returns the next transformed token. This process may also skip input
	 * tokens (e.g. comment tokens) depending on the configuration of the
	 * normalization.
	 */
	@Override
	protected TokenUnit provideNext() throws CloneDetectionException {
		if (identifierManager == null) {
			initRegions(tokenResourceRoot);
		}

		IToken token = getNextNonIgnoredToken();
		if (token == null) {
			// no more tokens, done!
			return null;
		}

		// update identifier normalization
		if (token.getLanguage().getStatementOracle()
				.isEndOfStatementToken(token.getType(), tokenProvider)) {
			previousToken = null;
			identifierManager.clear();
		}

		// handle sentinels
		if (token.getType() == ETokenType.SENTINEL) {
			previousToken = null;
			return new SentinelUnit(token.getOriginId());
		}

		int currentIndexInFile = indexInFile++;
		TokenUnit normalizedTokenUnit = createNormalizedTokenUnit(token,
				currentIndexInFile);
		handleEndOfFile(token);
		previousToken = token;
		return normalizedTokenUnit;
	}

	/** Creates the normalized token unit. */
	private TokenUnit createNormalizedTokenUnit(IToken token, int indexInElement) {
		ETokenType normalizedType = normalizeType(token);
		TokenUnit unit = new TokenUnit(normalizeContent(token, normalizedType),
				token.getText(), token.getOffset(), token.getEndOffset(),
				token.getOriginId(), normalizedType, indexInElement);
		normalizationMapping.put(token, unit);
		return unit;
	}

	/** Returns the next token that is not ignored. */
	private IToken getNextNonIgnoredToken() throws CloneDetectionException {
		IToken token = tokenProvider.getNext();
		while (token != null && isIgnored(token)) {
			handleEndOfFile(token);
			normalizationMapping.put(token, null);
			previousToken = token;
			token = tokenProvider.getNext();
		}
		return token;
	}

	/** Check if next token if in other file and clean up */
	private void handleEndOfFile(IToken token) throws CloneDetectionException {
		if (!inSameFile(token, tokenProvider.lookahead(1))) {

			// write debug file if last unit of file reached
			if (!StringUtils.isEmpty(debugFileExtension)) {
				writeDebugFile(token);
			}

			identifierManager.clear();
			indexInFile = 0;
			normalizationMapping.clear();
		}
	}

	/** Writes the normalization debug file. */
	private void writeDebugFile(IToken token) throws CloneDetectionException {
		ITokenElement element = getElement(token.getOriginId());
		CanonicalFile baseFile = ResourceUtils.getFile(element);
		if (baseFile == null) {
			throw new CloneDetectionException(
					"Can not create debug file as underlying system does not reside in file system!");
		}
		NormalizationDebugUtils.writeDebugFile(baseFile, element,
				normalizationMapping, getLogger(), debugFileExtension);
	}

	/**
	 * Returns the location for a uniform path using {@link #tokenResourceRoot}
	 * for lookup. The lookup map is cached in {@link #uniformPathToElement}.
	 */
	private ITokenElement getElement(String uniformPath) {
		if (uniformPathToElement == null) {
			uniformPathToElement = ResourceTraversalUtils
					.createUniformPathToElementMap(tokenResourceRoot,
							ITokenElement.class);
		}
		return uniformPathToElement.get(uniformPath);
	}

	/** Checks whether two tokens are in the same file */
	private boolean inSameFile(IToken token, IToken token2) {
		if (token == null || token2 == null) {
			return false;
		}
		return token.getOriginId().equals(token2.getOriginId());
	}

	/** Determines whether this token is to be ignored */
	private boolean isIgnored(IToken token) throws CloneDetectionException {

		if (alwaysKeepEndOfStatementTokens && isEndOfStatementToken(token)) {
			ignoreNextTokens = 0; // we don't ignore tokens in this case.
			return false;
		}

		if (ignoreNextTokens > 0) {
			ignoreNextTokens--;
			return true;
		}

		return isIgnoredEndOfStatementToken(token) || isIgnoredComment(token)
				|| isIgnoredDelimiter(token)
				|| isIgnoredPreprocessorDirective(token)
				|| isIgnoredThisReference(token)
				|| isIgnoredFullyQualifiedNameHead(token)
				|| isIgnoredModifier(token) || isIgnoredStopWord(token);
	}

	/** Determines whether a token is an end of statement token */
	private boolean isEndOfStatementToken(IToken token)
			throws CloneDetectionException {
		return token.getLanguage().getStatementOracle()
				.isEndOfStatementToken(token.getType(), tokenProvider);
	}

	/** Determines whether this token is an ignored End of Statement Token */
	private boolean isIgnoredEndOfStatementToken(IToken token)
			throws CloneDetectionException {
		boolean endOfStatementTokenRemovalEnabled = !alwaysKeepEndOfStatementTokens
				&& getConfigurationForToken(token)
						.isIgnoreEndOfStatementTokens();
		return endOfStatementTokenRemovalEnabled
				&& isEndOfStatementToken(token);
	}

	/** Determines whether this token is an ignored comment */
	private boolean isIgnoredComment(IToken token) {
		return token.getType().getTokenClass() == ETokenClass.COMMENT
				&& getConfigurationForToken(token).isIgnoreComments();
	}

	/**
	 * Determines whether this token is an ignored delimiter
	 * <p>
	 * 
	 */
	private boolean isIgnoredDelimiter(IToken token) {

		if (!getConfigurationForToken(token).isIgnoreDelimiters()) {
			return false;
		}

		// have to preserve '(' after C# using to allow proper
		// normalization/filtering (see CR#6254)
		if (previousToken != null
				&& previousToken.getType() == ETokenType.USING) {
			return false;
		}

		return token.getType() == ETokenType.LPAREN
				|| token.getType() == ETokenType.RPAREN
				|| token.getType() == ETokenType.LBRACK
				|| token.getType() == ETokenType.RBRACK;
	}

	/** Determines whether this token is an ignored preprocessor directive */
	private boolean isIgnoredPreprocessorDirective(IToken token) {
		return token.getType() == ETokenType.PREPROCESSOR_DIRECTIVE
				&& getConfigurationForToken(token)
						.isIgnorePreprocessorDirectives();
	}

	/** Determines whether this token is an ignored <em>this</em> reference */
	private boolean isIgnoredThisReference(IToken token)
			throws CloneDetectionException {
		if (token.getType() != ETokenType.THIS
				|| !getConfigurationForToken(token).isIgnoreThis()) {
			return false;
		}
		IToken nextToken = tokenProvider.lookahead(1);
		if (nextToken == null
				|| (nextToken.getType() != ETokenType.DOT && nextToken
						.getType() != ETokenType.POINTERTO)) {
			return false;
		}
		ignoreNextTokens = 1;
		return true;
	}

	/**
	 * Checks whether the token is the head of a fully qualified name. If so,
	 * the entire fully qualified name head (i.e. everything up to the last
	 * identifier) is ignored in one step.
	 */
	private boolean isIgnoredFullyQualifiedNameHead(IToken token)
			throws CloneDetectionException {
		if (token.getType() != ETokenType.IDENTIFIER
				|| !getConfigurationForToken(token)
						.isNormalizeFullyQualifiedNames()) {
			return false;
		}

		boolean isDot = lookaheadTokenType(1, ETokenType.DOT);
		if (!isDot && !lookaheadTokenType(1, ETokenType.SCOPE)) {
			return false;
		}

		// look for end of fq header
		ETokenType fqSeparatorTokenType = ETokenType.DOT;
		if (!isDot) {
			fqSeparatorTokenType = ETokenType.SCOPE;
		}

		int position = 1;
		while (lookaheadTokenType(position + 1, ETokenType.IDENTIFIER)
				&& lookaheadTokenType(position + 2, fqSeparatorTokenType)) {
			position += 2;
		}
		ignoreNextTokens = position;

		return true;
	}

	/** Tests whether the token is an ignored visibility modifier. */
	private boolean isIgnoredModifier(IToken token) {
		if (!getConfigurationForToken(token).isIgnoreModifier()) {
			return false;
		}
		return token.getType() == ETokenType.PUBLIC
				|| token.getType() == ETokenType.PROTECTED
				|| token.getType() == ETokenType.INTERNAL
				|| token.getType() == ETokenType.PRIVATE
				|| token.getType() == ETokenType.FINAL
				|| token.getType() == ETokenType.OVERRIDE;
	}

	/** Tests whether the token is an ignored stop word. */
	private boolean isIgnoredStopWord(IToken token) {
		ITokenConfiguration configuration = getConfigurationForToken(token);

		if (!configuration.isIgnoreStopWords()) {
			return false;
		}
		CCSMAssert
				.isNotNull(configuration.getStopWords(),
						"Stop word elimination is enabled, but no stop words are found!");

		return token.getType() == ETokenType.WORD
				&& configuration.getStopWords().isStopWord(token.getText());
	}

	/** Performs lookahead and checks whether the found token has a certain type */
	private boolean lookaheadTokenType(int index, ETokenType tokenType)
			throws CloneDetectionException {
		IToken token = tokenProvider.lookahead(index);
		return token != null && token.getType() == tokenType;
	}

	/** Normalizes the content of the token */
	private String normalizeContent(IToken token, ETokenType tokenType) {
		String content = token.getText();
		if (!token.getLanguage().isCaseSensitive()
				|| tokenType != ETokenType.IDENTIFIER) {
			content = content.toLowerCase();
		}

		ITokenConfiguration tokenConfig = getConfigurationForToken(token);

		switch (tokenType) {
		case IDENTIFIER:
			if (tokenConfig.isNormalizeIdentifiers()) {
				content = "id" + identifierManager.obtainId(content);
			}
			break;

		case STRING_LITERAL:
			if (tokenConfig.isNormalizeStringLiterals()) {
				content = StringUtils.EMPTY_STRING;
			}
			break;

		case CHARACTER_LITERAL:
			if (tokenConfig.isNormalizeCharacterLiterals()) {
				content = "char";
			}
			break;

		case INTEGER_LITERAL:
			if (tokenConfig.isNormalizeNumberLiterals()) {
				content = "0";
			}
			break;

		case FLOATING_POINT_LITERAL:
			if (tokenConfig.isNormalizeNumberLiterals()) {
				content = "0.0";
			}
			break;

		case BOOLEAN_LITERAL:
			if (tokenConfig.isNormalizeBooleanLiterals()) {
				content = "true";
			}
			break;

		case WORD:
			if (tokenConfig.isStemWords()) {
				CCSMAssert.isNotNull(tokenConfig.getStemmer(),
						"Stemming is enabled, but no stemmer has been set!");
				content = tokenConfig.getStemmer().stem(content);
			}
			break;

		case NUMBER_WORD:
			if (tokenConfig.isNormalizeNumberLiterals()) {
				content = "0";
			}
			break;

		case LINE:
			content = content.replaceAll("\\s+", "");

		default:
			// All other tokens will not be altered.
		}

		return content;
	}

	/**
	 * Gets the {@link ITokenConfiguration} determining the normalization for
	 * this token. The token-based choice of a configuration allows for
	 * context-sensitive normalization.
	 */
	private ITokenConfiguration getConfigurationForToken(IToken token) {
		for (String configName : configurations.keySet()) {
			RegionSet regions = configurationMap.getValue(token.getOriginId(),
					configName);
			if (regions != null && regions.contains(token.getOffset())) {
				return configurations.get(configName);
			}
		}

		return defaultConfig;
	}

	/** Normalize the type of a token */
	private ETokenType normalizeType(IToken token) {
		ETokenType tokenType = token.getType();

		if (!getConfigurationForToken(token).isNormalizeTypeKeywords()) {
			return tokenType;
		}

		if (tokenType == ETokenType.STRING
				&& token.getLanguage().equals(ELanguage.COBOL)) {
			return ETokenType.STRING; // is verb in COBOL and not a type
		}

		if (TYPE_KEYWORDS.contains(tokenType)) {
			return ETokenType.IDENTIFIER;
		}

		return tokenType;
	}

	/** Sets flag that turns off filtering of end of statement tokens */
	public void setAlwaysKeepEndOfStatementTokens(
			boolean alwaysKeepEndOfStatementTokens) {
		this.alwaysKeepEndOfStatementTokens = alwaysKeepEndOfStatementTokens;
	}

	/**
	 * Get the underlying token provider. Retrieving tokens from here changes
	 * the state of the normalization.
	 */
	public ITokenProvider getTokenProvider() {
		return tokenProvider;
	}

}