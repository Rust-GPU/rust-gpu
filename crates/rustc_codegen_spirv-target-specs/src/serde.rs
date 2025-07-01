use crate::SpirvTargetEnv;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

impl Serialize for SpirvTargetEnv {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.target_triple())
    }
}

impl<'de> Deserialize<'de> for SpirvTargetEnv {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let target = String::deserialize(deserializer)?;
        Self::parse_triple(&target).map_err(|_e| serde::de::Error::unknown_variant(&target, &[]))
    }
}

#[cfg(test)]
mod tests {
    use crate::SpirvTargetEnv;

    #[test]
    fn test_serde_roundtrip() {
        for env in SpirvTargetEnv::iter() {
            let json = serde_json::to_string(&env).unwrap();
            let deserialize: SpirvTargetEnv = serde_json::from_str(&json).unwrap();
            assert_eq!(env, deserialize);
        }
    }
}
