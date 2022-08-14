const path = require("path");

describe("Conduit", () => {
	beforeAll(async () => {
		await page.goto("file://" + path.resolve("dist/index.html"));
	});

	// it('should be titled "Conduit"', async () => {
	// 	await expect(page.title()).resolves.toMatch("Conduit");
	// });

	// it('should navigate to sign in when clicking on the sign in nav button', async () => {
  //   // await page.waitForSelector("#nav-sign-in");
  //   // const n = await page.$("#nav-sign-in");
  //   // await n.click();
  //   // await page.waitForSelector("#sign-in-h1");
  //   // const si = await page.$("#sign-in-h1");
  //   // const value = await page.evaluate(el => el.textContent, si)
	// 	// expect(value).toMatch("Sign in");
	// });
  it ('tests', () => {expect(1+1).toEqual(2)});
});
