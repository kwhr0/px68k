#include <cstdint>
#include <deque>

struct CompareProcess {
	CompareProcess() : compareMode(false) {}
	virtual void Stop() {}
	void setCompare(bool f) {
		compareMode = f;
		if (!compareMode) { rlog.clear(); wlog.clear(); }
	}
	bool WriteStart(uint32_t adr, uint16_t data, uint16_t size) {
		adr &= 0xffffff;
		if (!size) data &= 0xff;
		if (!compareMode)
			wlog.emplace_back(adr, data, size);
		else {
			if (wlog.empty()) {
				fprintf(stderr, "wlog is empty.\n");
				Stop();
			}
			Acs acs = wlog.front();
			wlog.pop_front();
			if (adr != acs.adr) {
				fprintf(stderr, "write address error: cur=%x new=%x\n", acs.adr, adr);
				Stop();
			}
			if (data != acs.data) {
				fprintf(stderr, "write data error: cur=%x new=%x\n", acs.data, data);
				Stop();
			}
			if (size != acs.size) {
				fprintf(stderr, "write size error: cur=%x new=%x\n", acs.size, size);
				Stop();
			}
			return true;
		}
		return false;
	}
	bool ReadStart(uint32_t adr, uint16_t &data, uint16_t size) {
		adr &= 0xffffff;
		if (compareMode) {
			if (rlog.empty()) {
				fprintf(stderr, "rlog is empty.\n");
				Stop();
			}
			Acs acs = rlog.front();
			rlog.pop_front();
			if (adr != acs.adr) {
				fprintf(stderr, "read address error: cur=%x new=%x\n", acs.adr, adr);
				Stop();
			}
			if (size != acs.size) {
				fprintf(stderr, "read size error: cur=%x new=%x\n", acs.size, size);
				Stop();
			}
			data = acs.data;
			return true;
		}
		orgadr = adr;
		orgsize = size;
		return false;
	}
	void ReadEnd(uint16_t data) {
		if (!compareMode)
			rlog.emplace_back(orgadr, data, orgsize);
	}
	struct Acs {
		Acs(uint32_t a, uint16_t d, uint16_t s) : adr(a), data(d), size(s) {}
		uint32_t adr;
		uint16_t data, size;
	};
	std::deque<Acs> rlog, wlog;
	uint32_t orgadr, orgsize;
	bool compareMode;
};
