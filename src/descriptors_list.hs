module Descriptors where

import Data.Set (Set)
import qualified Data.Set as Set

type Descriptor = String
type DescriptorSet = Set Descriptor

haveDescriptor :: Descriptor -> DescriptorSet -> bool
haveDescriptor descriptor descriptor_set = Set.member descriptor descriptor_set

addDescriptor :: Descriptor -> DescriptorSet -> DescriptorSet
addDescriptor descriptor descriptor_set = Set.insert

initDescriptorSet :: DescriptorSet
initDescriptorSet = Set.empty

addCacheFile :: String -> DescriptorSet -> DescriptorSet
addCacheFile file_content descriptor_set =
