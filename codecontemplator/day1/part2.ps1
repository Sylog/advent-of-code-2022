$text = Get-Content .\input.txt | out-string
$nl = [System.Environment]::NewLine
$groups = $text -split "$nl$nl"

$groups | % {
    $_ -split "$nl" | % { [int]$_} | Measure-Object -Sum | Select-Object -ExpandProperty Sum
} | Sort-Object -Descending | Select -First 3 | Measure-Object -Sum 