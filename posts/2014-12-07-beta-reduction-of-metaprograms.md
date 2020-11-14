---
title: Beta reduction of metaprograms
---
Long I have wondered what's better and before I did wonder, I thought it was even pretty useful, preferable to have code in Exhibit Ņ in place of code in Exhibit Ŗ.
I'd call this code a type of metaprogramming. Stuff that is not about to explode in size, gets no input in the runtime and might as well be just expanded.

Exhibit Ņ

    class EmployeeInfoForm(forms.ModelForm):
        class Meta:
            model   = EmployeeInfo
            fields  = ['count', 'commentary']
            widgets = {'commentary': forms.Textarea()}

Exhibit Ŗ

    class EmployeeInfoFormOther(forms.ModelForm):
        def __init__(self, *args, **kwargs):
            super(EmployeeInfoFormOther, self).__init__(*args, **kwargs)
            self.fields['alt_role'] = forms.CharField()

As I said, I thought it was useful but it was only neat.

Well but DRY(!) one might say — Don't Repeat Yourself. Bullshit I say! Sure it's nice for computers to be able to compute, but what matters at the end of the day is data.
The same way we don't write 1+1 if we need just 2 why do we need to hide the data we want behind ugly computations we ourselves can compute?

DRY has its place and it's a valid conclusion, sometimes. Here there are only four lines in each class. And are they going to grow bigger? How about we take the easy way out and wait for more lines to come? The class might even never surpass 10 lines.

Let's compute what it should look like and see whether it's clearer. Exhibit N, R.

Exhibit N

    class EmployeeInfoForm(forms.ModelForm):
        class Meta:
            model   = EmployeeInfo
            fields  = ['count', 'commentary']
            widgets = {'commentary': forms.Textarea()}

Exhibit R

    class EmployeeInfoFormOther(forms.ModelForm):
        class Meta:
            model   = EmployeeInfo
            fields  = ['alt_role', 'count', 'commentary']
            widgets = {'commentary': forms.Textarea()}

Yes, clarity's all around! Only one line changed in between the five, but I'd still prefer this one.
