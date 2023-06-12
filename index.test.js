const jestConfig = require("./jest.config");

let server;
describe("Conduit", () => {
	beforeAll(async () => {
		const app = require("express")();
		app.use(require("express-static")("dist/"));
		server = await app.listen(3000);
		await page.goto("http://localhost:3000");
	});
 afterAll(async function () {
		// Cleanup
		await server.close();
 });
	it('should be titled "Conduit"', async () => {
		await expect(page.title()).resolves.toMatch("Conduit");
	});

	it('should navigate to sign in when clicking on the sign in nav button', async () => {
	  await page.waitForSelector("#nav-sign-in");
	  const n = await page.$("#nav-sign-in");
	  await n.click();
	  await page.waitForSelector("#sign-in-h1");
	  const si = await page.$("#sign-in-h1");
	  const value = await page.evaluate(el => el.textContent, si)
		expect(value).toMatch("Sign in");
	});

});
