module Descriptors where

-- Haskell modules

import Network

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Maybe

-- Local modules

import ConnectionList
import ReadCache (readCacheFile, getDescriptorList,
		  RessourceInformation, RessourceDescriptor,
		  getDescriptor)

type Descriptor = RessourceDescriptor
type DescriptorMap = Map HostName (Set Descriptor)

-- Return true if the descriptor_map has the descriptor
haveDescriptor :: HostName -> DescriptorMap -> Descriptor -> Bool
haveDescriptor conn_id descriptor_map descriptor = descriptor `Set.member` (descriptor_map Map.! conn_id)

-- Add descriptor inside the descriptor_map
addDescriptor :: HostName -> DescriptorMap -> Descriptor -> DescriptorMap
addDescriptor conn_id descriptor_map descriptor =
	let
		old_set = descriptor_map Map.! conn_id
	in
		Map.insert conn_id (Set.insert descriptor old_set) descriptor_map

-- Empty descriptor map
initDescriptorMap :: DescriptorMap
initDescriptorMap  = Map.empty

--
addCacheFile :: HostName -> DescriptorMap -> String -> Maybe DescriptorMap
addCacheFile conn_id descriptor_map file_content  =
	let
		parse_result :: Maybe [RessourceInformation]
		parse_result = readCacheFile file_content
	in
		case parse_result of
			Nothing ->
				Nothing
			Just ressource_infos ->
				Just $ foldl (addCacheFile_ conn_id) descriptor_map ressource_infos
		where
			addCacheFile_ :: HostName -> DescriptorMap -> RessourceInformation -> DescriptorMap
			addCacheFile_ conn_id desc_map ressource_info =
				addDescriptor conn_id desc_map (getDescriptor ressource_info)
