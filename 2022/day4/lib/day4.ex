defmodule Day4 do
  def is_first_in_range_second(first, second) do
    [a, b | _] = (first |> Enum.map(fn x -> Integer.parse x end))
    [c, d | _] = (second |> Enum.map(fn x -> Integer.parse x end))
    (c <= a) and (b <= d)
  end

  def do_they_overlap(first_elf, second_elf) do
    f = String.split(first_elf,"-")
    s = String.split(second_elf,"-")
    is_first_in_range_second(f, s) or is_first_in_range_second(s, f)
  end

  def start do
    IO.read(:stdio, :all)
    |> String.split("\n")
    |> Enum.map(fn x -> String.split(x, ",") end)
    |> Enum.filter(fn x -> x != [""] end)
    |> Enum.reduce(0, fn x, acc ->
      [elf_a, elf_b | _] = x

      if do_they_overlap(elf_a, elf_b) do
        acc + 1
      else
        acc
      end
    end)
    |> IO.puts()
  end
end
