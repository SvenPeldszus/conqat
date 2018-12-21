public class A {

		public void AddMapping<T>() where T: IConformistHoldersProvider, new()
		{
			AddMapping(new T());
		}
		
		public static V GetOrDefault<K,V>(this IReadOnlyDictionary<K, V> dict, K key)
		{
			V ret;
			dict.TryGetValue(key, out ret);
			return ret;
		}
		

}