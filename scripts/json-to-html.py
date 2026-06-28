#!/usr/bin/env python3

"""Convert exported JSON to an HTML visualization.
"""

import argparse
import html
import json
from pathlib import Path
from typing import Any, TextIO

SCRIPT_DIR = Path(__file__).resolve().parent

NODE_RESERVED_KEYS = frozenset({'$$data_type', 'type', 'ref', 'properties', 'contents'})
DOC_RESERVED_KEYS = frozenset({'$$data_type', 'properties', 'contents'})

CARD_HEADER_TYPES = frozenset({'headline', 'section'})


def join_secondary_string(sstring: list[Any]) -> str:
	return ' '.join(s if isinstance(s, str) else '<???>' for s in sstring)


def format_title(title: list[Any] | str | None) -> str:
	if title is None:
		return '<No title>'
	if isinstance(title, str):
		return title
	return join_secondary_string(title)


def load_asset(name: str) -> str:
	return (SCRIPT_DIR / name).read_text(encoding='utf-8')


def json_block(data: Any) -> str:
	encoded = html.escape(json.dumps(data, indent=2))
	return (
		f'<pre><code class="language-json">{encoded}</code></pre>'
	)


def render_json_block(label: str, data: Any) -> str:
	return (
		f'<div class="org-node-card__block">'
		f'<p class="org-node-card__block-label">{html.escape(label)}</p>'
		f'{json_block(data)}'
		f'</div>'
	)


def extra_fields(node: dict[str, Any], *, is_document: bool) -> dict[str, Any]:
	if is_document:
		reserved = DOC_RESERVED_KEYS
	else:
		reserved = NODE_RESERVED_KEYS

	fields = {key: value for key, value in node.items() if key not in reserved}

	# Omit empty extra fields on headlines
	if not is_document and node['type'] == 'headline':
		for prop in ['drawer', 'tags-all']:
			if prop in fields and not fields[prop]:
				del fields[prop]

	return fields


def render_text_block(text: str) -> str:
	return json_block(text)


def render_contents_block(children_html: str) -> str:
	return (
		f'<div class="org-node-card__block">'
		f'<p class="org-node-card__block-label">Contents</p>'
		f'<div class="org-node-card__children">{children_html}</div>'
		f'</div>'
	)


def render_content_item(item: Any, *, shallow: bool = False) -> str:
	if isinstance(item, str):
		return render_text_block(item)
	if isinstance(item, dict):
		return render_node_card(item, shallow=shallow)
	raise TypeError(f'Unexpected contents item: {type(item)!r}')


def card_header_class(node_type: str) -> str:
	if node_type in CARD_HEADER_TYPES:
		return f' org-node-card__header--{node_type}'
	return ''


def render_node_card(node: dict[str, Any], *, shallow: bool = False) -> str:
	is_document = node['$$data_type'] == 'org-document'
	node_type = node['$$data_type'] if is_document else node['type']
	ref = '—' if is_document else node.get('ref', '—')

	body_parts: list[str] = []

	properties = node.get('properties')
	if properties:
		body_parts.append(render_json_block('Properties', properties))

	extra = extra_fields(node, is_document=is_document)
	if extra:
		body_parts.append(render_json_block('Additional fields', extra))

	if not shallow:
		contents = node.get('contents') or []
		if contents:
			child_cards = ''.join(
				render_content_item(child, shallow=False) for child in contents
			)
			body_parts.append(render_contents_block(child_cards))

	body = ''.join(body_parts)
	header_class = f'org-node-card__header{card_header_class(node_type)}'
	return (
		f'<article class="org-node-card" data-type="{html.escape(node_type)}">'
		f'<header class="{header_class}">'
		f'<span class="org-node-card__type">{html.escape(node_type)}</span>'
		f'<span class="org-node-card__ref">{html.escape(ref)}</span>'
		f'</header>'
		f'<div class="org-node-card__body">{body}</div>'
		f'</article>'
	)


def render_heading(title: list[Any] | str | None, level: int) -> str:
	tag = f'h{level}'
	return f'<{tag}>{html.escape(format_title(title))}</{tag}>'


def render_headline_outline(headline: dict[str, Any]) -> str:
	assert headline['$$data_type'] == 'org-node'
	assert headline['type'] == 'headline'

	props = headline['properties']
	contents = headline.get('contents') or []
	parts = [
		render_heading(props.get('raw-value') or props.get('title'), props['level'] + 1),
		render_node_card(headline, shallow=True),
	]

	rest = contents
	if contents and contents[0]['type'] == 'section':
		section = contents[0]
		parts.append(render_node_card(section, shallow=True))
		for child in section.get('contents') or []:
			parts.append(render_node_card(child, shallow=False))
		rest = contents[1:]

	for item in rest:
		if item['type'] == 'headline':
			parts.append(render_headline_outline(item))
		else:
			parts.append(render_node_card(item, shallow=False))

	return ''.join(parts)


def render_children(contents: list[dict[str, Any]]) -> str:
	if not contents:
		return ''

	assert all(child['$$data_type'] == 'org-node' for child in contents), 'Expected org-node objects'

	parts: list[str] = []
	rest = contents

	if contents[0]['type'] == 'section':
		section = contents[0]
		parts.append(render_node_card(section, shallow=True))
		for child in section.get('contents') or []:
			parts.append(render_node_card(child, shallow=False))
		rest = contents[1:]

	for item in rest:
		if item['type'] == 'headline':
			parts.append(render_headline_outline(item))
		else:
			parts.append(render_node_card(item, shallow=False))

	return ''.join(parts)


def render_document(data: dict[str, Any]) -> str:
	assert data['$$data_type'] == 'org-document'

	contents = data.get('contents') or []
	parts = [
		render_heading(data['properties'].get('title'), 1),
		render_node_card(data, shallow=True),
		render_children(contents),
	]
	return ''.join(parts)


def render_page(data: dict[str, Any]) -> str:
	template = load_asset('json-to-html.template.html')
	css = load_asset('json-to-html.css')
	title = format_title(data['properties'].get('title'))
	body = render_document(data)

	return (
		template
		.replace('{{title}}', html.escape(title))
		.replace('{{css}}', css)
		.replace('{{body}}', body)
	)


def export(data: dict[str, Any], file: TextIO) -> None:
	file.write(render_page(data))


def make_parser() -> argparse.ArgumentParser:
	desc = __doc__.split('\n\n')[0]  # pyright: ignore[reportOptionalMemberAccess]
	parser = argparse.ArgumentParser(description=desc)
	parser.add_argument('json_file', type=argparse.FileType('r'), help='The JSON file to convert', default='-')
	parser.add_argument('out_file', type=argparse.FileType('w'), help='The output HTML file', default='-')
	return parser


def main() -> None:
	parser = make_parser()
	args = parser.parse_args()

	with args.json_file:
		data = json.load(args.json_file)

	export(data, args.out_file)


if __name__ == '__main__':
	main()
