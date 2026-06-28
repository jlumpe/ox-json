#!/usr/bin/env python3

"""Convert exported JSON to simple Markdown.
"""

import json
import argparse
from typing import TextIO, Any
import sys



def write_json(data: Any, file: TextIO) -> None:
	file.write('```json\n')
	json.dump(data, file, indent=2)
	file.write('\n```\n')


def join_secondary_string(sstring: list[Any]) -> str:
	return ' '.join(s if isinstance(s, str) else '<???>' for s in sstring)


def make_title(title: list[Any] | str | None, level: int) -> str:
	if title is None:
		title_str = '<No title>'
	elif isinstance(title, str):
		title_str = title
	else:
		title_str = join_secondary_string(title)

	return '#' * level + ' ' + title_str


def write_section(section: dict[str, Any], file: TextIO) -> None:
	contents = section.pop('contents', [])

	file.write('Section properties:\n\n')
	write_json(section, file)
	file.write('\n\n')

	if contents:
		file.write('Section contents:\n\n')
		for item in contents:
			write_json(item, file)
			file.write('\n\n')


def write_children(contents: list[dict[str, Any]], file: TextIO) -> None:

	if not contents:
		return

	assert all(c['$$data_type'] == 'org-node' for c in contents), 'Expected org-node objects'

	if contents[0]['type'] == 'section':
		write_section(contents[0], file)
		contents = contents[1:]

	for item in contents:
		write_headline(item, file)


def write_headline(data: dict[str, Any], file: TextIO) -> None:
	assert data['$$data_type'] == 'org-node', 'Expected org-node object'
	assert data['type'] == 'headline', 'Expected headline node'

	props = data['properties']
	contents = data.pop('contents', [])

	file.write(make_title(props.get('raw-value') or props.get('title'), props['level'] + 1))
	file.write('\n\nHeadline properties:\n\n')
	write_json(data, file)
	file.write('\n\n')

	write_children(contents, file)


def export(data: dict[str, Any], file: TextIO) -> None:

	assert data['$$data_type'] == 'org-document', 'Expected org-document object'

	contents = data.pop('contents', [])

	# Title
	file.write(make_title(data['properties'].get('title'), 1))

	# File properties
	file.write('\n\nFile properties:\n\n')
	write_json(data, file)
	file.write('\n\n')

	# Contents
	write_children(contents, file)


def make_parser() -> argparse.ArgumentParser:
	desc = __doc__.split('\n\n')[0]  # pyright: ignore[reportOptionalMemberAccess]
	parser = argparse.ArgumentParser(description=desc)
	parser.add_argument('json_file', type=argparse.FileType('r'), help='The JSON file to convert', default='-')
	parser.add_argument('out_file', type=argparse.FileType('w'), help='The output Markdown file', default='-')
	return parser


def main():
	parser = make_parser()
	args = parser.parse_args()

	with args.json_file:
		data = json.load(args.json_file)

	export(data, args.out_file)


if __name__ == '__main__':
	main()
