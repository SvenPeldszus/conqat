public class Foo
{
    private IEnumerable<ValidationResult> ValidateName(IPropertyBinding arg)
    {
        IEnumerable<string> allNames = ViewModel.AllNames;
        string name = (string)ViewModel.NameBinding.Value;

        if (allNames != null && allNames.Count(n => string.Compare(n, name, true, SomeInfo.InvariantThing) == 0) > 1)
        {
            yield return ValidationResult.Create(ValidationMethodResult.Fail("Name must be unique."),
                Severity.SevereError, false);
        }
    }
}
