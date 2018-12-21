def id(self): pass

def _load_players(self, savegame_db, force_player_id):
	human_players = []
	for player_worldid, client_id in savegame_db("SELECT rowid, client_id FROM player WHERE is_trader = 0 and is_pirate = 0 ORDER BY rowid"):
		player = None
		# check if player is an ai
		ai_data = self.session.db("SELECT class_package, class_name FROM ai WHERE client_id = ?", client_id)
		if ai_data:
			class_package, class_name = ai_data[0]
			# import ai class and call load on it
			module = __import__('horizons.ai.'+class_package, fromlist=[str(class_name)])
			ai_class = getattr(module, class_name)
			player = ai_class.load(self.session, savegame_db, player_worldid)
		else: # no ai
			player = HumanPlayer.load(self.session, savegame_db, player_worldid)
		self.players.append(player)

		if client_id == horizons.globals.fife.get_uh_setting("ClientID"):
			self.player = player
		elif client_id is not None and not ai_data:
			# possible human player candidate with different client id
			human_players.append(player)
	self.owner_highlight_active = False
	self.health_visible_for_all_health_instances = False

	if self.player is None:
		# we have no human player.
		# check if there is only one player with an id (i.e. human player)
		# this would be the case if the savegame originates from a different installation.
		# if there's more than one of this kind, we can't be sure what to select.
		# TODO: create interface for selecting player, if we want this
		if len(human_players) == 1:
			# exactly one player, we can quite safely use this one
			self.player = human_players[0]
		elif not human_players and self.players:
			# the first player should be the human-ai hybrid
			self.player = self.players[0]

	# set the human player to the forced value (debug option)
	self.set_forced_player(force_player_id)

	if self.player is None and self.session.is_game_loaded():
		self.log.warning('WARNING: Cannot autoselect a player because there '
		                 'are no or multiple candidates.')