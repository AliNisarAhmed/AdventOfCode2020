const fs = require('fs');

async function getData(): Promise<string> {
	return new Promise((resolve, reject) => {
		fs.readFile('../data/1.txt', 'utf-8', (err, data) => {
			if (err) {
				reject('File read error');
			} else {
				resolve(data);
			}
		})
	})

}

async function run() {
	const data = await getData();
	const numbers = data.split('\n').map(Number).sort((a, b) => a - b);

	let i = 0;
	let j = numbers.length - 1;

	while (i < j) {
		const sum = numbers[i] + numbers[j];

		if (sum === 2020) {
			return numbers[i] * numbers[j];
		}

		if (sum < 2020) {
			i++;
		}

		if (sum > 2020) {
			j--
		}
	}
}

async function two() {
	const data = await getData();
	const numbers = data.split('\n').map(Number).sort((a, b) => a - b);

	let i = 0;
	let j = numbers.length - 1;
	let k = i + 1;

	while (i < j) {
		while (k < j) {
			if (numbers[i] + numbers[j] + numbers[k] === 2020) {
				return numbers[i] * numbers[j] * numbers[k];
			}

			k++;
		}

		i++;
		j--;
		k = i + 1;
	}
}

run().then(r => console.log(r))
two().then(r => console.log(r))